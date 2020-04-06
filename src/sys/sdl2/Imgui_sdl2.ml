
open Ctypes
open Tsdl

module Types = Imgui_sdl2_ffi_f.Types
module Ffi = Imgui_sdl2_ffi_f.Make(Imgui_sdl2_generated_funs)

let _unwrap = function
  | Ok x -> x
  | Error (`Msg e) -> failwith @@ "sdl error " ^ e

let create_window_d3d ?(w=800) ?(h=800) (flags:Sdl.Window.flags) : Types.window ptr =
  let w = Sdl.create_window "sdl" ~w ~h flags |> _unwrap in
  let w = Ctypes.coerce Ctypes.nativeint
      (ptr Types.window) @@ Sdl.unsafe_ptr_of_window w in
  if not @@ Ffi.init_for_d3d w then (
    failwith "failed during initialization of D3D SDL_Window";
  );
  w

(** @param gl_ctx the openGL context *)
let create_window_opengl ?(w=800) ?(h=800) (flags:Sdl.Window.flags)
    ~(gl_ctx:unit ptr) : Types.window ptr =
  let w = Sdl.create_window "sdl" ~w ~h flags |> _unwrap in
  let w = Ctypes.coerce Ctypes.nativeint
      (ptr Types.window) @@ Sdl.unsafe_ptr_of_window w in
  if not @@ Ffi.init_for_opengl w gl_ctx then (
    failwith "failed during initialization of openGL SDL_Window";
  );
  w

let create_window_vulkan ?(w=800) ?(h=800) (flags:Sdl.Window.flags) : Types.window ptr =
  let w = Sdl.create_window "sdl" ~w ~h flags |> _unwrap in
  let w = Ctypes.coerce Ctypes.nativeint
      (ptr Types.window) @@ Sdl.unsafe_ptr_of_window w in
  if not @@ Ffi.init_for_vulkan w then (
    failwith "failed during initialization of vulkan SDL_Window";
  );
  w

let new_frame = Ffi.new_frame
let process_event = Ffi.process_event

