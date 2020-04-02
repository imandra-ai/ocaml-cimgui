let prefix = "imgui_stubs"

let c_prelude = {|
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS 1
#include "cimgui.h"

|}

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
    assert !genc;
    print_endline c_prelude;
    Cstubs.write_c Format.std_formatter ~prefix (module Funs_ffi.Make)
  )
