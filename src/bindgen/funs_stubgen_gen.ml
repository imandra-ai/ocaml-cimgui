
module J = Yojson.Safe
module JU = J.Util

let (|?) o x = match o with Some y -> y | None -> x
let (%>) f g x = g (f x)
let spf = Printf.sprintf
let pf = Printf.printf
let pfl fmt = Printf.kfprintf (fun oc -> output_char oc '\n') stdout fmt

let path_json = "../../vendor/cimgui/generator/output/definitions.json"

let is_upper c = c = Char.uppercase_ascii c
let escape_c_ s =
  let buf = Buffer.create (String.length s + 10) in
  (* turn "*" into " * " to avoid producing OCaml comments *)
  String.iter
    (function '*' -> Buffer.add_string buf " * " | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* use [Foreign.funptr] to be able to pass OCaml functions *)
let funptr =
  "let open Ctypes in Foreign.funptr"

let () =
  let j = Yojson.Safe.from_file path_json in
  Printf.eprintf "parsed json\n%!";
  (* load type definitions *)
  let graph = Ty_g.of_file "types.data" in

  let buf = Buffer.create 256 in
  let bpf fmt = Printf.bprintf buf fmt in
  let bpfl fmt = Printf.kbprintf (fun buf -> Buffer.add_char buf '\n') buf fmt in

  let mk_ml_name cname = if is_upper cname.[0] then "f_" ^ cname else cname in

  pfl "open Ctypes";
  pfl "module Types = Imgui_generated_types.Make(Generated_types)";
  pfl "module Make (F : Cstubs.FOREIGN) = struct";
  pfl "  open F";
  pfl "  open Types";

  let handle_def_const cname d =
    let ml_name = mk_ml_name cname in
    try
      let ty_ret = JU.member "ret" d |> JU.to_string in
      let ret, _ = Ty_g.parse_ty ~funptr graph ty_ret in
      pfl "  let %s = foreign %S (void @-> returning %s)" ml_name cname ret;
    with e ->
      pfl "  let _f_%s = [`Skipped]\n  (* omitted: constant %s:\n    %s *)"
        (mk_ml_name cname) cname (Printexc.to_string e);
  in
  let rec handle_def_fun cname d ty_args ?spec_for ty_ret =
    let vararg = List.mem "isvararg" @@ JU.keys d in
    if vararg && spec_for=None then (
      (* specialize for up to 4 args *)
      for i = 0 to 4 do
        pfl "  (* specialize variadic %s for %d arguments *)" cname i;
        handle_def_fun cname d ty_args ~spec_for:i ty_ret
      done;
    ) else (
      try
        Buffer.clear buf;
        let ml_name = match spec_for with
          | None | Some 0 -> mk_ml_name cname
          | Some n -> spf "%s%d" (mk_ml_name cname) n
        in
        bpfl "\n  (** function %s\n   args: [%s] *)"
          (JU.member "funcname" d |> JU.to_string)
          (escape_c_ (JU.member "args" d|>JU.to_string));
        bpf "  let %s = foreign %S (" ml_name cname;
        List.iter
          (fun arg ->
             let ty = JU.member "type" arg |> JU.to_string in
             if vararg && ty = "..." then (
               (* add n arguments of type string *)
               let n = match spec_for with Some n -> n | None -> assert false in
               for _i=1 to n do
                 bpf "(* varargs spec *) string @-> ";
               done;
             ) else (
               (* translate type, where "*" is an optional ptr and arrays
                  of fixed size are pointers *)
               let ty', _ =
                 Ty_g.parse_ty ~funptr ~array_to_ptr:true ~ptr_top:"ptr_opt" graph ty
               in
               bpf "(%s) @-> " ty'
             ))
          ty_args;
        let ty, _ = Ty_g.parse_ty ~funptr graph ty_ret in
        bpfl " returning (%s))" ty;
        print_endline @@ Buffer.contents buf
      with e ->
        pfl "  (* skip definition of %s:\n  %s *)" cname (Printexc.to_string e);
        ()
    )
  in

  let handle_def name d : unit =
    let cname = JU.member "ov_cimguiname" d |> JU.to_string in
    Printf.eprintf "handle def %s for cimguiname %s\n%!" name cname;
    let ty_args = JU.member "argsT" d |> JU.to_list in
    if ty_args=[] then (
      handle_def_const cname d
    ) else (
      let cstor =
        try JU.member "constructor" d |> JU.to_bool with _ -> false in
      (* constructor=true means ty_ret is the struct itself *)
      let ty_ret =
        if cstor then JU.member "stname" d |> JU.to_string
        else JU.member "ret" d |> JU.to_string
      in
      (* FIXME: handle constructors *)
      if cstor then pfl "  (* skip cstor %s *)" name else
      handle_def_fun cname d ty_args ty_ret
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

