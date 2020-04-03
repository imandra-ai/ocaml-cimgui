
let prelude = {|
  #include <stddef.h>
  //#include "imgui.h"
  #include "cimgui.h"
  #include "cimgui_impl.h"
  union anon_union1 {
    int val_i; float val_f; void* val_p;
  };
  |}

let () =
  print_endline prelude;
  Cstubs_structs.write_c Format.std_formatter (module Imgui_generated_types.Make);
  ()
