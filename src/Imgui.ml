
module Ctypes = Ctypes
module I = Imgui_sys
include Imgui_sys

open Ctypes

module Infix = struct
  let (!@) = (!@)
  (** Dereference the pointer *)

  let (<-@) = (<-@)
  (** Set the pointer's content *)

  let (@.) = (@.)
  (** Access a structure field *)
end
include Infix

(** A pointer to a Ctypes value *)
type 'a ptr = 'a Ctypes.ptr

let setf = Ctypes.setf
let getf = Ctypes.getf
let allocate = Ctypes.allocate

let vec2 x y : ImVec2.t =
  let v2 = make ImVec2.t in
  setf v2 ImVec2.x x;
  setf v2 ImVec2.y y;
  v2

let vec4 x y z w : ImVec4.t =
  let v = make ImVec4.t in
  setf v ImVec4.x x;
  setf v ImVec4.y y;
  setf v ImVec4.z z;
  setf v ImVec4.w w;
  v

(** Create main menu bar, calls [f()] if it's enabled. *)
let main_menu_bar ~f =
  if I.igBeginMainMenuBar () then (
    f ();
    I.igEndMainMenuBar();
  )

(** Create menu list, calls [f()] if it's enabled. *)
let menu (label:string) (active:bool) ~f : unit =
  if I.igBeginMenu label active then (
    f();
    I.igEndMenu();
  )

let menu_item_bool ?(shortcut="") (label:string) ~f : unit =
  if I.igMenuItemBool label shortcut false true then f ()

(** Create a button, call [f()] if it's clicked *)
let button ?dim (label:string) ~f =
  let dim = match dim with
    | None -> vec2 (float_of_int @@ String.length label * 18) 18.
    | Some d -> d
  in
  if I.igButton label dim then (
    f()
  )

