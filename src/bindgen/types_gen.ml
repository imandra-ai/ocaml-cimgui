
module J = Yojson.Safe
module JU = J.Util

let (%>) f g x = g (f x)
let spf = Printf.sprintf
let pf = Printf.printf
let pfl fmt = Printf.kfprintf (fun oc -> output_char oc '\n') stdout fmt

let path_json_typedefs = "../../vendor/cimgui/generator/output/typedefs_dict.json"
let path_json_enums_structs = "../../vendor/cimgui/generator/output/structs_and_enums.json"

module Str_ = struct
  let lsplit_on_char c s =
    try
      (* find `c`*)
      let i = String.index s c in
      String.sub s 0 i
    with _ -> s

  let rsplit_on_char c s =
    try
      (* find `c` that is not trailing *)
      let i = String.rindex_from s (String.length s-2) c in
      String.sub s (i+1) (String.length s-i-1)
    with _ -> s

  let prefix s1 s2 =
    String.length s2 >= String.length s1 &&
    try for i=0 to String.length s1-1 do
        if s1.[i] <> s2.[i] then raise Exit;
      done; true
    with Exit -> false

  let contains_at_ ~sub i s j ~len =
    let rec check k =
      if k = len
      then true
      else sub.[i+k] = s.[j+k] && check (k+1)
    in
    j+len <= String.length s && check 0

  let contains s1 s2 =
    let rec try_at i =
      i + String.length s1 <= String.length s2 &&
      (contains_at_ ~sub:s1 0 s2 i ~len:(String.length s1) ||
       try_at (i+1))
    in
    try_at 0
end

type any_ty = Any : 'a Ctypes.typ -> any_ty

let () =
  let tydefs = Yojson.Safe.from_file path_json_typedefs |> JU.to_assoc in
  let ml_names = Hashtbl.create 32 in

  (* translate a type *)
  let parse_ty s =
    let rec try_prim ~fdef s =
      match s with
      | "unsigned char" -> "uchar"
      | "unsigned short" -> "ushort"
      | "unsigned int" -> "uint"
      | "int" -> "int"
      | "short" -> "short"
      | "char" -> "char"
      | "float" -> "float"
      | "double" -> "double"
      | "bool" -> "bool"
      | "const char*" -> "string"
      | "void*" | "const void*" -> "voidp"
      | s when s.[String.length s-1] = '*' ->
        let s = String.sub s 0 (String.length s-1) in
        let ty = expand_ty ~fdef s in
        spf "(ptr %s)" ty
      | s when Str_.prefix "ImVector_" s ->
        spf "(abstract ~name:%S ~size:%d ~alignment:8)" s (3 * 8)
      | s when Str_.prefix "const " s ->
        (* drop const *)
        let lenc = String.length "const " in
        expand_ty ~fdef (String.sub s lenc (String.length s-lenc))
      | s when Str_.prefix "struct " s ->
        (* drop struct *)
        let lenc = String.length "struct " in
        expand_ty ~fdef (String.sub s lenc (String.length s-lenc))
      | _ -> lookup_ty s
    and expand_ty ~fdef s =
      if fdef then
        match List.assoc s tydefs |> JU.to_string with
        | s2 -> try_prim ~fdef:false s2
        | exception Not_found -> try_prim ~fdef:false s
      else try_prim ~fdef s
    and lookup_ty s =
      try Hashtbl.find ml_names s
      with Not_found ->
        Printf.eprintf "cannot find name %S" s;
        failwith "cannot translate"
    in
    expand_ty ~fdef:true s
  in

  let j = Yojson.Safe.from_file path_json_enums_structs in
  pfl "open Ctypes";
  pfl "module Make(S : Cstubs_structs.TYPE) = struct";
  pfl "  open S";
  pfl "let voidp = ptr (abstract ~name:\"void\" ~size:8 ~alignment:8)";
  (* graph of type name -> declaration, with dependencies *)
  let decls = ref [] in
  let mods = ref [] in
  let buf = Buffer.create 256 in
  let add_decl name ml_name =
    if Hashtbl.mem ml_names name then (
      failwith (spf "double declaration for %s -> %s" name ml_name);
    );
    Hashtbl.add ml_names name ml_name;
    decls := Buffer.contents buf :: !decls; Buffer.clear buf
  and add_mod f =
    mods := f :: !mods
  in
  let bpfl fmt = Printf.kbprintf (fun buf -> Buffer.add_char buf '\n') buf fmt in
  let handle_enum name args =
    Printf.eprintf "handle enum %s\n%!" name;
    Buffer.clear buf;
    bpfl "module %s = struct" name;
    let args = JU.to_list args in
    let c_cstors = List.map (JU.member "name" %> JU.to_string) args in
    let ml_cstors = List.map (Str_.rsplit_on_char '_') c_cstors in
    bpfl "  type t = %s" (String.concat " | " ml_cstors);
    bpfl "  let t : t typ = enum %S [" name;
    List.iter2
      (fun ml_c c_c ->
         bpfl "    (%s, constant %S int64_t);" ml_c c_c)
      ml_cstors c_cstors;
    bpfl "  ]";
    bpfl "end";
    add_decl name (spf "%s.t" name); (* pure decl *)
  in
  (* declare as opaque *)
  let handle_opaque name =
    Printf.eprintf "handle opaque %s\n%!" name;
    Buffer.clear buf;
    (* TODO *)
    bpfl "module %s = struct" name;
    bpfl "  type t = [`%s] abstract" name;
    bpfl "  let t : t typ = abstract ~name:%S ~size:8 ~alignment:8" name;
    bpfl "end";
    add_decl name (spf "%s.t" name);
  in
  let handle_struct_normal name args : unit =
    Printf.eprintf "handle struct normal %s\n%!" name;
    Buffer.clear buf;
    bpfl "module Decl_%s = struct" name;
    bpfl "  type t = [`%s] Ctypes.structure" name;
    bpfl "  let t : t typ = structure %S" name;
    bpfl "end";
    add_decl name (spf "Decl_%s.t" name);
    add_mod (fun () ->
      bpfl "module %s = struct" name;
      bpfl "  include Decl_%s" name; (* reuse decl *)
      List.iter
        (fun p ->
           let f_name = JU.member "name" p |> JU.to_string in
           let f_type = JU.member "type" p |> JU.to_string in
           if String.contains f_name '[' then (
             (* array! *)
             let f_name = Str_.lsplit_on_char '[' f_name in
             let f_size = JU.member "size" p |> JU.to_int in
             let f_ty = parse_ty f_type in
             let f_ty = spf "(array %d %s)" f_size f_ty in
             bpfl "  let f_%s = field t %S %s" f_name f_name f_ty;
           ) else (
             let f_ty = parse_ty f_type in
             bpfl "  let f_%s = field t %S %s" f_name f_name f_ty;
           )
        )
        args;
      bpfl "  let () = seal t";
      bpfl "end";
    );
  in
  let handle_struct name args =
    let args = JU.to_list args in
    let must_abstract =
      List.exists
        (fun p ->
           let f_name = JU.member "name" p |> JU.to_string in
           let f_type = JU.member "type" p |> JU.to_string in
           (* deref if needed *)
           let f_type' = try List.assoc f_type tydefs |> JU.to_string with _ -> f_type in
           (* avoid: unions, function pointers *)
           f_name = "" || Str_.contains "(*)" f_type || Str_.contains "(*)" f_type')
        args
    in
    if must_abstract then (
      (* anonymous field, so an union: skip *)
      handle_opaque name
    ) else (
      handle_struct_normal name args
    )
  in
  let handle_enums l =
    List.iter (fun (name, args) -> handle_enum name args) l
  and handle_structs l =
    List.iter (fun (name, args) -> handle_struct name args) l
  in
  (* NOTE: missing types *)
  List.iter handle_opaque
    ["ImDrawListSharedData"; "ImDrawIdx"; "ImWchar"];
  (* traverse [j] *)
  handle_enums (JU.to_assoc @@ JU.member "enums" j);
  handle_structs (JU.to_assoc @@ JU.member "structs" j);
  List.iter print_endline !decls;
  List.iter
    (fun f -> Buffer.clear buf; f(); print_endline @@ Buffer.contents buf)
    !mods;
  pf "end;;\n%!";
  ()
