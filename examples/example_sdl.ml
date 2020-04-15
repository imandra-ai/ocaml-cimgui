
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

(** @param gl_ctx the openGL context *)
let create_window_opengl3 ?(w=800) ?(h=800) () : _ ptr * _ * I.ImGuiContext.t ptr =
  let open Imgui_sdl2 in
  (* TODO: provide these flags somewhere else *)
  let flags = Sdl.Window.(opengl + resizable + allow_highdpi) in
  let sdl_w = Sdl.create_window "sdl" ~w ~h flags |> unwrap_ in
  let w = get_w_ptr sdl_w in
  let gl_ctx = Sdl.gl_create_context sdl_w |> unwrap_ in
  Sdl.gl_make_current sdl_w gl_ctx |> unwrap_;
  if get_gl_ctx gl_ctx |> is_null then failwith "sdl init: gl_context is null";

  let ctx = I.igCreateContext None in
  let io = I.igGetIO () in

  if not @@ Ffi.init_for_opengl w (get_gl_ctx gl_ctx) then (
    failwith "failed during initialization of openGL SDL_Window";
  );

  Printf.printf "init imgui opengl\n%!";
  if not @@ Imgui_opengl3.init None then (
    failwith "failed to initialize imgui<->openGL3"
  );
  Printf.printf "imgui opengl initialized\n%!";
  w, io, ctx

let () =
  Printexc.record_backtrace true;
  let open I.ImGuiIO in

  (* FIXME
  Printf.printf "create sdl window\n%!";
  let win, _gl_ctx = Imgui_sdl2.create_window_opengl Sdl.Window.windowed in

  Printf.printf "init imgui opengl\n%!";
  if not @@ Imgui_opengl3.init None then (
    failwith "failed to initialize imgui<->openGL3"
  );
  Printf.printf "imgui opengl initialized\n%!";
     *)

  let win, io, ctx = create_window_opengl3 () in

(* TODO
  setf !@io f_DisplaySize (mk_vec2 1920. 1080.);
  setf !@io f_DisplaySize (mk_vec2 1920. 1080.);
*)
  let fonts = getf !@io f_Fonts in
  let tex_w = allocate int 0 in
  let tex_h = allocate int 0 in
  let tex_pixels =
    allocate (ptr uchar) (coerce (ptr void) (ptr uchar) null) in
  let bytes_per_pixels = allocate int 0 in
  I.f_ImFontAtlas_GetTexDataAsRGBA32 (Some fonts)
    (Some tex_pixels) (Some tex_w) (Some tex_h) (Some bytes_per_pixels);
  I.igStyleColorsDark None;

  let my_f = allocate float 0. in

  for i = 0 to 200 do
    Printf.printf "new frame (%d)\n%!" i;
    Imgui_opengl3.new_frame();
    Printf.printf "new frame opengl ✔\n%!";
    Imgui_sdl2.new_frame win;
    Printf.printf "new frame sdl ✔\n%!";
    I.igNewFrame();
    Printf.printf "new frame IG ✔\n%!";

    (* TODO: poll *)

    I.igText "hello world!";
    let slider_changed =
      I.igSliderFloat "float slider" (Some my_f) 0. 1. "%.3f" 1.0
    in
    if slider_changed then Printf.printf "slider touched! val=%.3f\n%!" !@my_f;
    (* TODO?
       I.igShowDemoWindow (Some (allocate bool true)); *)

    I.igRender();
    Unix.sleepf (1. /. 24.);
    ()
  done;

  Imgui_opengl3.shutdown();
  (* TODO: destroy sdl context *)
  I.igDestroyContext (Some ctx);
  ()
