
module Types = Imgui_opengl3_ffi_f.Types
module Ffi = Imgui_opengl3_ffi_f.Make(Imgui_opengl3_generated_funs)

let init = Ffi.init
let shutdown = Ffi.shutdown
let new_frame = Ffi.new_frame
let render_data = Ffi.render_data
