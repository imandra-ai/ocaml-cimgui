
open Ctypes
open Tsdl

module Types = Imgui_sdl2_ffi_f.Types
module Ffi = Imgui_sdl2_ffi_f.Make(Imgui_sdl2_generated_funs)

let unwrap_ = function
  | Ok x -> x
  | Error (`Msg e) -> failwith @@ "sdl error " ^ e

(** Cast: get a window pointer *)
let get_w_ptr (w:Sdl.window) : Types.window ptr =
  Ctypes.coerce (ptr void)
    (ptr Types.window) @@ ptr_of_raw_address @@ Sdl.unsafe_ptr_of_window w

(** Cast: get a unit pointer *)
let get_gl_ctx (ctx:Sdl.gl_context) : unit ptr =
  ptr_of_raw_address @@ Sdl.unsafe_ptr_of_gl_context ctx

let create_window_d3d ?(w=800) ?(h=800) (flags:Sdl.Window.flags) : Types.window ptr =
  let w = Sdl.create_window "sdl" ~w ~h flags |> unwrap_ |> get_w_ptr in
  if not @@ Ffi.init_for_d3d w then (
    failwith "failed during initialization of D3D SDL_Window";
  );
  w

(** @param gl_ctx the openGL context *)
let create_window_opengl
    ?(w=800) ?(h=800) (flags:Sdl.Window.flags) : Types.window ptr * Sdl.gl_context =
  (* TODO: provide these flags somewhere else *)
  let flags = Sdl.Window.(flags + opengl + resizable + allow_highdpi) in
  let sdl_w = Sdl.create_window "sdl" ~w ~h flags |> unwrap_ in
  let w = get_w_ptr sdl_w in
  let gl_ctx = Sdl.gl_create_context sdl_w |> unwrap_ in
  Sdl.gl_make_current sdl_w gl_ctx |> unwrap_;
  if get_gl_ctx gl_ctx |> is_null then failwith "sdl init: gl_context is null";
  if not @@ Ffi.init_for_opengl w (get_gl_ctx gl_ctx) then (
    failwith "failed during initialization of openGL SDL_Window";
  );
  w, gl_ctx

let create_window_vulkan ?(w=800) ?(h=800) (flags:Sdl.Window.flags) : Types.window ptr =
  let w = Sdl.create_window "sdl" ~w ~h flags |> unwrap_ |> get_w_ptr in
  if not @@ Ffi.init_for_vulkan w then (
    failwith "failed during initialization of vulkan SDL_Window";
  );
  w

let new_frame = Ffi.new_frame
let process_event = Ffi.process_event

