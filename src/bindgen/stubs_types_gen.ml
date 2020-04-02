
let prelude = {|
  #include "imgui"
  |}

let () =
  print_endline prelude;
  Cstubs_structs.write_c Format.std_formatter (module Stubs_types.Make);
  ()
