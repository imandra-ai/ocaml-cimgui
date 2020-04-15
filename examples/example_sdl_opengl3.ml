
open Ctypes
open Tgl3
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
let create_window_opengl3 ?(w=800) ?(h=800) () =
  let open Imgui_sdl2 in
  (* TODO: provide these flags somewhere else *)
  let flags = Sdl.Window.(opengl + resizable + allow_highdpi) in
  let sdl_w = Sdl.create_window "sdl" ~w ~h flags |> unwrap_ in
  let w = get_w_ptr sdl_w in
  let gl_ctx = Sdl.gl_create_context sdl_w |> unwrap_ in
  Sdl.gl_make_current sdl_w gl_ctx |> unwrap_;
  Sdl.gl_set_swap_interval 1 |> sdl_u_; (* v-sync *)
  if get_gl_ctx gl_ctx |> is_null then failwith "sdl init: gl_context is null";

  (match Imgui_opengl3.glewInit() with
     | Ok () -> ()
     | Error e -> Printf.printf "error in glew init: %s\n%!" e;
  );

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
  w, sdl_w, io, ctx

let () =
  Printexc.record_backtrace true;
  let open I.ImGuiIO in
  Sdl.init Sdl.Init.(video + timer + gamecontroller) |> Imgui_sdl2.unwrap_;

  let win, sdl_win, io, ctx = create_window_opengl3 () in

  setf !@io f_DisplaySize (mk_vec2 1920. 1080.);
  let fonts = getf !@io f_Fonts in
  let tex_w = allocate int 0 in
  let tex_h = allocate int 0 in
  let tex_pixels =
    allocate (ptr uchar) (coerce (ptr void) (ptr uchar) null) in
  let bytes_per_pixels = allocate int 0 in
  I.f_ImFontAtlas_GetTexDataAsRGBA32 (Some fonts)
    (Some tex_pixels) (Some tex_w) (Some tex_h) (Some bytes_per_pixels);
  I.igStyleColorsDark None;

  let i = ref 0 in
  let sdl_event = Sdl.Event.create () in
  let my_f = allocate float 0. in
  let demo_win = allocate bool false in
  let simple_win = ref false in
  let quit = ref false in

  while not !quit do
    (* poll and handle events *)
    while Sdl.poll_event (Some sdl_event) do
      (* NOTE: unsafe for now *)
      let ok = Imgui_sdl2.process_event (Imgui_sdl2.get_sdl_event sdl_event) in
      if not ok then Printf.printf "imgui: fail to process event\n%!";
      if Sdl.Event.(get sdl_event typ) = Sdl.Event.quit then (
        quit := true
      ) else if Sdl.Event.(get sdl_event typ) = Sdl.Event.window_event &&
                Sdl.Event.(get sdl_event window_event_id) =
                Sdl.Event.window_event_close
                  (* FIXME
                &&
                Sdl.Event.(get sdl_event window_window_id 
                  *)
      then (
        quit := true;
      )
    done;

    (* TODO: change this? to resize dynamically?
    setf !@io f_DisplaySize
      (let w,h = sdl_u_ @@ Sdl.get_renderer_output_size
         @@ sdl_u_ @@ Sdl.get_renderer sdl_win
       in
       (mk_vec2 (float_of_int w) (float_of_int h)));
       *)

    incr i;
    Printf.printf "new frame (%d)\n%!" !i;
    Imgui_opengl3.new_frame();
    Printf.printf "new frame opengl ✔\n%!";
    Imgui_sdl2.new_frame win;
    Printf.printf "new frame sdl ✔\n%!";
    I.igNewFrame();
    Printf.printf "new frame IG ✔\n%!";

    I.igText (Printf.sprintf "hello world! (frame %d)" !i);
    let slider_changed =
      I.igSliderFloat "float slider" (Some my_f) 0. 1. "%.3f" 1.0
    in
    if slider_changed then Printf.printf "slider touched! val=%.3f\n%!" !@my_f;
    if I.igCheckbox "show demo window" (Some demo_win) then (
      Printf.printf "checked boxed for demo window";
    );
    if I.igButton "show other window" (mk_vec2 60. 60.) then (
      Printf.printf "display other window";
      simple_win := true;
    );
    if !@ demo_win then (
      I.igShowDemoWindow (Some demo_win);
    );

    if !simple_win then (
      ignore_bool @@ I.igBegin "simple window" None 0;
      I.igText "tada! 🎉";
      if I.igButton "close me" (mk_vec2 60. 60.) then simple_win := false;
      I.igEnd();
    );

    I.igRender();
    Gl.viewport
      0 0
      (int_of_float @@ getf (getf !@io f_DisplaySize) I.ImVec2.x)
      (int_of_float @@ getf (getf !@io f_DisplaySize) I.ImVec2.y);
    Gl.clear Gl.color_buffer_bit;
    Imgui_opengl3.render_data (I.igGetDrawData ());
    Sdl.gl_swap_window sdl_win;
    Unix.sleepf (1. /. 24.);
    ()
  done;

  Printf.printf "shutdown…\n%!";
  Imgui_opengl3.shutdown();
  Imgui_sdl2.shutdown ();
  I.igDestroyContext (Some ctx);
  Sdl.destroy_window sdl_win;
  Sdl.quit();
  Printf.printf "bye!\n%!";
  ()
