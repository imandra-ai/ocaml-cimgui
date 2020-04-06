

module Make(Types : Cstubs_structs.TYPE) = struct
  open! Types
  open Ctypes_static
  type window = [`SDL_Window] abstract
  let window : window typ =
    abstract ~name:"SDL_Window" ~size:8 ~alignment:8 (* do not use directly *)

  type event = [`SDL_Event] abstract
  let event : event typ =
    abstract ~name:"SDL_Event" ~size:8 ~alignment:8 (* do not use directly *)
end
