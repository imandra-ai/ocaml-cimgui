
module Ctypes = Ctypes
include Imgui_sys

open Ctypes

module Infix = struct
  let (!@) = (!@)
  let (<-@) = (<-@)
end
include Infix

type 'a ptr = 'a Ctypes.ptr
let allocate = Ctypes.allocate

let vec2 x y =
  let v2 = make ImVec2.t in
  setf v2 ImVec2.x x;
  setf v2 ImVec2.x y;
  v2
