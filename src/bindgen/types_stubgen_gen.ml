
module J = Yojson.Safe
module JU = J.Util

let (%>) f g x = g (f x)
let spf = Printf.sprintf
let pf = Printf.printf
let pfl fmt = Printf.kfprintf (fun oc -> output_char oc '\n') stdout fmt

let path_json_typedefs = "../../vendor/cimgui/generator/output/typedefs_dict.json"
let path_json_enums_structs = "../../vendor/cimgui/generator/output/structs_and_enums.json"

let strip_last_underscore (s:string) : string =
  assert (s <> "");
  let n = String.length s in
  if s.[n-1] = '_' then String.sub s 0 (n-1) else s

let is_upper c = c = Char.uppercase_ascii c

let () =
  Printexc.record_backtrace true;
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
    (* for some reason cimgui adds a "_" at the end of the type… *)
    let name = strip_last_underscore name in
    Printf.eprintf "handle enum %s\n%!" name;
    let ml_name = spf "%s.t" name in
    let code = lazy (
      Buffer.clear buf;
      bpfl "module %s = struct" name;
      let args = JU.to_list args in
      let c_cstors =
        List.map
          (fun c ->
             JU.member "name" c |> JU.to_string,
             JU.member "calc_value" c |> JU.to_int)
          args in
      let ml_cstors, ml_cstors_with_vals =
        List.map
          (fun (name,v) ->
             let ml_name = Str_.rsplit_on_char '_' name in
             ml_name, Printf.sprintf "%s (** value %d *) " ml_name v)
          c_cstors
        |> List.split
      in
      bpfl "  type t = %s" (String.concat " | " @@ ml_cstors_with_vals);
      bpfl "  let t : t typ = enum ~typedef:true %S [" name;
      List.iter2
        (fun ml_c (c_c,_) ->
           bpfl "    (%s, constant %S int64_t);" ml_c c_c)
        ml_cstors c_cstors;
      bpfl "  ]";
      (* the integer values *)
      List.iter2
        (fun ml_c (_c_c,c_val) ->
           bpfl "  let int_%s = %d" ml_c c_val)
        ml_cstors c_cstors;
      (* TODO: emit some "lor" operator*)
      bpfl "end";
      Buffer.contents buf, []
    ) in
    Ty_g.add_decl ~enum:true graph name ~code ~ml_name;
  in
  (* declare as opaque *)
  let handle_opaque reason name =
    let ml_name = spf "%s.t" name in
    let code = lazy (
      Buffer.clear buf;
      bpfl "module %s = struct" name;
      bpfl "  type t = [`%s] abstract" name;
      bpfl "  (* skip for reason: %s *)" reason;
      bpfl "  let t : t typ = abstract ~name:%S ~size:8 ~alignment:8" name;
      bpfl "end";
      Buffer.contents buf, []
    ) in
    Ty_g.add_decl graph name ~ml_name ~code;
  in
  let handle_struct name args =
    let args = JU.to_list args in
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
           let mk_ml_name f_name =
             if f_name = "" then "_anon_field"
             else if is_upper f_name.[0] then "f_" ^ f_name else f_name
           in
           if String.contains f_name '[' then (
             (* array! *)
             let f_name = Str_.lsplit_on_char '[' f_name in
             let f_size = JU.member "size" p |> JU.to_int in
             let f_ty, deps' = Ty_g.parse_ty graph f_type in
             deps := deps' @ !deps;
             let f_ty = spf "(array %d %s)" f_size f_ty in
             bpfl "  let %s = field t %S %s" (mk_ml_name f_name) f_name f_ty;
           ) else (
             let f_ty, deps' = Ty_g.parse_ty graph f_type in
             deps := deps' @ !deps;
             let f_name =
               if f_name = ""
               then "val_i"  (* NOTE: gross hack to find offset of only anonymous union *)
               else f_name in
             bpfl "  let %s = field t %S %s" (mk_ml_name f_name) f_name f_ty;
             );
        )
        args;
      bpfl "  let () = seal t";
      bpfl "end";
      Buffer.contents buf, !deps
    ) in
    Ty_g.add_def graph ("$$def-"^name) ~code ~ml_name;
  in
  let handle_enums l =
    List.iter (fun (name, args) -> handle_enum name args) l
  and handle_structs l =
    List.iter (fun (name, args) -> handle_struct name args) l
  in
  (* NOTE: missing types *)
  List.iter (handle_opaque "missing type decl")
    ["ImDrawListSharedData"; "ImDrawIdx"; "ImWchar"; "ImGuiContext";
    ];
  print_endline Ty_g.def_unions;
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
