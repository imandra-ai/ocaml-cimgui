
open Ctypes
module Sdl = Tsdl.Sdl

module I = Imgui_sys

let sdl_u_ = function
  | Ok x -> x
  | Error (`Msg e) -> failwith @@ "sdl error " ^ e

let mk_vec2 x y =
  let v2 = make I.ImVec2.t in
  setf v2 I.ImVec2.x x;
  setf v2 I.ImVec2.x y;
  v2

let ignore_bool : bool -> unit = ignore

let () =
  let open I.ImGuiIO in
  let _ctx = I.igCreateContext None in
  let io = I.igGetIO () in

  (* TODO *)
  let _w = Imgui_sdl2.create_window_vulkan Sdl.Window.windowed in

  setf !@io f_DisplaySize (mk_vec2 1920. 1080.);
  setf !@io f_DisplaySize (mk_vec2 1920. 1080.);
  let fonts = getf !@io f_Fonts in
  let tex_w = allocate int 0 in
  let tex_h = allocate int 0 in
  let tex_pixels =
    allocate (ptr uchar) (coerce (ptr void) (ptr uchar) null) in
  let bytes_per_pixels = allocate int 0 in
  I.f_ImFontAtlas_GetTexDataAsRGBA32 (Some fonts)
    (Some tex_pixels) (Some tex_w) (Some tex_h) (Some bytes_per_pixels);

  let my_f = allocate float 0. in

  for i = 0 to 200 do
    I.igNewFrame();
    Printf.printf "new frame %d\n%!" i;

    I.igText "hello world!";
    let slider_changed =
      I.igSliderFloat "float slider" (Some my_f) 0. 1. "%.3f" 1.0
    in
    if slider_changed then Printf.printf "slider touched! val=%.3f\n%!" !@my_f;
    I.igShowDemoWindow None;

    I.igRender();
    ()
  done;

  I.igDestroyContext (Some _ctx);
  ()
