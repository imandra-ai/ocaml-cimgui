
let path_json = "../../vendor/cimgui/generator/output/definitions.json"

let bindgen ~dir : unit =
  let j = Yojson.Safe.from_file path_json in
  print_endline "parsed json";
  (* TODO *)
  let oc = open_out "imgui_sys.ml" in
  Printf.fprintf oc "let () = ()";
  let oc = open_out "imgui_sys.mli" in
  Printf.fprintf oc "";
  ()


let () =
  let dir = ref "." in
  let opts = [
    "-d", Arg.Set_string dir, " output directory";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> raise (Arg.Help "no positional args")) "bindgen";
  bindgen ~dir:!dir
