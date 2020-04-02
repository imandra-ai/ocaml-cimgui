let prefix = "imgui_stubs"

let genml = ref false
let genc = ref false
let () =
 Arg.parse [
    "-ml", Arg.Set genml, " generate ml";
    "-c", Arg.Set genc, " generate c";
    ] (fun _ -> ()) "stubgen";
  if !genml = !genc then failwith {|specify exactly one of -ml/-c|};
  if !genml then (
    Cstubs.write_ml Format.std_formatter ~prefix (module Funs_ffi.Make)
  ) else (
    Cstubs.write_c Format.std_formatter ~prefix (module Funs_ffi.Make)
  )
