
module J = Yojson.Safe
module JU = J.Util

let (|?) o x = match o with Some y -> y | None -> x
let (%>) f g x = g (f x)
let spf = Printf.sprintf
let pf = Printf.printf
let pfl fmt = Printf.kfprintf (fun oc -> output_char oc '\n') stdout fmt

let path_json = "../../vendor/cimgui/generator/output/definitions.json"

let () =
  let j = Yojson.Safe.from_file path_json in
  Printf.eprintf "parsed json\n%!";
  (* load type definitions *)
  let graph = Ty_g.of_file "types.data" in

  let buf = Buffer.create 256 in
  let bpf fmt = Printf.bprintf buf fmt in
  let bpfl fmt = Printf.kbprintf (fun buf -> Buffer.add_char buf '\n') buf fmt in

  pfl "open Ctypes";
  pfl "module Types = Types_stubs_generated.Make(Generated_types)";
  pfl "module Make (F : Cstubs.FOREIGN) = struct";
  pfl "  open F";
  pfl "  open Types";

  let handle_def_const cname =
    Printf.eprintf "handle constant def for cimguiname %s\n%!" cname;
    () (* TODO *)
  in
  let handle_def_fun cname ty_args ty_ret =
    Printf.eprintf "handle fun def for cimguiname %s (%d args)\n%!"
      cname (List.length ty_args);
    try
      Buffer.clear buf;
      bpf "  let f_%s = foreign %S (" cname cname;
      List.iter
        (fun arg ->
           let ty = JU.member "type" arg |> JU.to_string in
           let ty, _ = Ty_g.parse_ty graph ty in
           bpf "(%s) @-> " ty)
        ty_args;
      let ty, _ = Ty_g.parse_ty graph ty_ret in
      bpfl " returning (%s))" ty;
      print_endline @@ Buffer.contents buf
    with e ->
      Printf.eprintf "skip definition of %s: error %s\n%!"
        cname (Printexc.to_string e);
      ()
  in

  let handle_def name d : unit =
    let cname = JU.member "ov_cimguiname" d |> JU.to_string in
    Printf.eprintf "handle def %s for cimguiname %s\n%!" name cname;
    let ty_args = JU.member "argsT" d |> JU.to_list in
    if ty_args=[] then (
      handle_def_const cname
    ) else (
      let cstor =
        try JU.member "constructor" d |> JU.to_bool with _ -> false in
      (* constructor=true means ty_ret is the struct itself *)
      let ty_ret =
        if cstor then name
        else JU.member "ret" d |> JU.to_string
      in
      handle_def_fun cname ty_args ty_ret
    )
  in

  (* traverse definitions *)
  let l = JU.to_assoc j in
  List.iter
    (fun (name, defs) ->
       List.iter (fun d -> handle_def name d) (JU.to_list defs))
    l;

  pfl "end";
  ()

