
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

let () =
  let tydefs =
    Yojson.Safe.from_file path_json_typedefs |> JU.to_assoc
    |> List.map (fun (a,b) -> a, JU.to_string b)
  in
  let graph = Ty_g.create ~tydefs () in


  let j = Yojson.Safe.from_file path_json_enums_structs in
  pfl "open Ctypes";
  pfl "module Make(S : Cstubs_structs.TYPE) = struct";
  pfl "  open S";
  pfl "let voidp = ptr void";
  (* graph of type name -> declaration, with dependencies *)
  let buf = Buffer.create 256 in
  let bpfl fmt = Printf.kbprintf (fun buf -> Buffer.add_char buf '\n') buf fmt in
  let handle_enum name args =
    Printf.eprintf "handle enum %s\n%!" name;
    let ml_name = spf "%s.t" name in
    let code = lazy (
      Buffer.clear buf;
      bpfl "module %s = struct" name;
      let args = JU.to_list args in
      let c_cstors = List.map (JU.member "name" %> JU.to_string) args in
      let ml_cstors = List.map (Str_.rsplit_on_char '_') c_cstors in
      bpfl "  type t = %s" (String.concat " | " ml_cstors);
      bpfl "  let t : t typ = enum ~typedef:true %S [" name;
      List.iter2
        (fun ml_c c_c ->
           bpfl "    (%s, constant %S int64_t);" ml_c c_c)
        ml_cstors c_cstors;
      bpfl "  ]";
      bpfl "end";
      Buffer.contents buf, []
    ) in
    Ty_g.add_decl graph name ~code ~ml_name;
  in
  (* declare as opaque *)
  let handle_opaque name =
    Printf.eprintf "handle opaque %s\n%!" name;
    let ml_name = spf "%s.t" name in
    let code = lazy (
      Buffer.clear buf;
      bpfl "module %s = struct" name;
      bpfl "  type t = [`%s] abstract" name;
      bpfl "  let t : t typ = abstract ~name:%S ~size:8 ~alignment:8" name;
      bpfl "end";
      Buffer.contents buf, []
    ) in
    Ty_g.add_decl graph name ~ml_name ~code;
  in
  let handle_struct_normal name args : unit =
    Printf.eprintf "handle struct normal %s\n%!" name;
    (* first, declare the struct *)
    let ml_name = spf "Decl_%s.t" name in
    let code = lazy (
      Buffer.clear buf;
      let deps = ref [] in
      bpfl "module Decl_%s = struct" name;
      bpfl "  type t = [`%s] Ctypes.structure" name;
      bpfl "  let t : t typ = structure %S" name;
      bpfl "end";
      Buffer.contents buf, !deps
    ) in
    Ty_g.add_decl graph name ~code ~ml_name;
    (* second, define the struct *)
    let ml_name = spf "%s.t" name in
    let code = lazy (
      Buffer.clear buf;
      let deps = ref [Ty_g.Dep_decl name] in
      bpfl "module %s = struct" name;
      bpfl "  include Decl_%s" name;
      List.iter
        (fun p ->
           let f_name = JU.member "name" p |> JU.to_string in
           let f_type = JU.member "type" p |> JU.to_string in
           if String.contains f_name '[' then (
             (* array! *)
             let f_name = Str_.lsplit_on_char '[' f_name in
             let f_size = JU.member "size" p |> JU.to_int in
             let f_ty, deps' = Ty_g.parse_ty graph f_type in
             deps := deps' @ !deps;
             let f_ty = spf "(array %d %s)" f_size f_ty in
             bpfl "  let f_%s = field t %S %s" f_name f_name f_ty;
           ) else (
             let f_ty, deps' = Ty_g.parse_ty graph f_type in
             deps := deps' @ !deps;
             bpfl "  let f_%s = field t %S %s" f_name f_name f_ty;
           )
        )
        args;
      bpfl "  let () = seal t";
      bpfl "end";
      Buffer.contents buf, !deps
    ) in
    Ty_g.add_def graph ("$$def-"^name) ~code ~ml_name;
  in
  let handle_struct name args =
    let args = JU.to_list args in
    let must_abstract =
      List.exists
        (fun p ->
           let f_name = JU.member "name" p |> JU.to_string in
           let f_type = JU.member "type" p |> JU.to_string in
           (* deref if needed *)
           let f_type' = try List.assoc f_type tydefs with _ -> f_type in
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
  List.iter
    (fun code -> print_endline code)
    (Ty_g.sorted graph);
  pf "end;;\n%!";
  Printf.eprintf "store types in types.data\n%!";
  Ty_g.to_file graph "types.data";
  ()
