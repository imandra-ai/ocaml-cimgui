
let prelude = {|
  #include <stddef.h>
  //#include "imgui.h"
  #include "cimgui.h"
  #include "cimgui_impl.h"
  |}

let () =
  print_endline prelude;
  Cstubs_structs.write_c Format.std_formatter (module Imgui_generated_types.Make);
  ()
