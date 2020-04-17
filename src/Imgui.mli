
(** {1 Main API for imgui} *)

module Ctypes = Ctypes
include module type of Imgui_sys

val vec2 : float -> float -> ImVec2.t

type 'a ptr = 'a Ctypes.ptr
(** A pointer to a Ctypes value *)

val allocate : ?finalise:('a ptr -> unit) -> 'a Ctypes.typ -> 'a -> 'a ptr

module Infix : sig
  val (!@) : 'a ptr -> 'a
  (** Dereference the pointer *)

  val (<-@) : 'a ptr -> 'a -> unit
  (** Set the pointer's content *)
end

include module type of Infix
