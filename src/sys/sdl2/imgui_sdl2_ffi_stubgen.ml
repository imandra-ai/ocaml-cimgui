
let prelude =
  {|
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS 1
#include "SDL.h"
#include "cimgui.h"
#include "cimgui_impl.h"
#include <stdbool.h>
|}

let () =
  let genml = ref false in
  let genc = ref false in
  Arg.parse [
    "-ml", Arg.Set genml, " gen ml";
    "-c", Arg.Set genc, " gen c";
  ] (fun _ -> failwith "bluh") "";
  if !genml = !genc then failwith "give exactly one arg";
  let prefix = "cimgui_ml_sdl2" in
  if !genml then (
    Cstubs.write_ml Format.std_formatter ~prefix (module Imgui_sdl2_ffi_f.Make)
  ) else (
    print_endline prelude;
    Cstubs.write_c Format.std_formatter ~prefix (module Imgui_sdl2_ffi_f.Make)
  )
