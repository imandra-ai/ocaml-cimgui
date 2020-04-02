
let prelude = {|
  #include "cimgui.h"
  #include "cimgui_impl.h"
  |}

let () =
  print_endline prelude;
  Cstubs_structs.write_c Format.std_formatter (module Stubs_types.Make);
  ()
