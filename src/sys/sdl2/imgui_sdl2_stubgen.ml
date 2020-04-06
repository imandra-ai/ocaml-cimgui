
module Types = struct

end

module Make(Ctypes : Cstubs.FOREIGN) = struct
  open Ctypes

  type window = [`Window] abstract
  let window : window typ =
    abstract ~name:"window" ~size:8 ~alignment:8 (* do not use *)

  type window_ptr = window ptr
  let window_ptr : window_ptr typ =
    view nativeint
      ~write:(fun p -> raw_address_of_ptr @@ to_voidp p)
      ~read:(fun addr -> let p = ptr_of_raw_address addr in from_voidp window p)

  let init_for_d3d =
    foreign "ImGui_ImplSDL2_InitForD3D"
      (window_ptr @-> returning bool)


end
