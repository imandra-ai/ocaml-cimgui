let prelude = {|
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS 1
#include "cimgui.h"
#include "GL/gl.h"
|}

let () =
  print_endline prelude;
  Cstubs_structs.write_c Format.std_formatter (module Imgui_opengl3_types_f.Make)
