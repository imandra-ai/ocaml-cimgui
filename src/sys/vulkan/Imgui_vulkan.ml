
module Types = Imgui_vulkan_ffi_f.Types
module Ffi = Imgui_vulkan_ffi_f.Make(Imgui_vulkan_generated_funs)

let init = Ffi.init
let shutdown = Ffi.shutdown
let new_frame = Ffi.new_frame
