
open Ctypes
open Tgl3
module Sdl = Tsdl.Sdl
module I = Imgui

let sdl_u_ = function
  | Ok x -> x
  | Error (`Msg e) -> failwith @@ "sdl error: " ^ e

let log_ = ref false

module State = struct
  open Imgui_sys_sdl2
  type t = {
    sdl_w: Sdl.window;
    w: Types.window ptr;
    gl_ctx: Sdl.gl_context;
    ctx: I.ImGuiContext.t ptr;
    io: I.ImGuiIO.t ptr;
    sdl_event: Sdl.event;
  }

  let init ?w:(width=800) ?(h=800) ?(flags=Sdl.Window.opengl)
      () : t =
    Sdl.init Sdl.Init.(video + timer + gamecontroller) |> sdl_u_;

    let flags = Sdl.Window.(opengl + resizable + allow_highdpi + flags) in
    let sdl_w = Sdl.create_window "sdl" ~w:width ~h flags |> unwrap_ in
    let w = get_w_ptr sdl_w in
    let gl_ctx = Sdl.gl_create_context sdl_w |> unwrap_ in
    Sdl.gl_make_current sdl_w gl_ctx |> unwrap_;
    Sdl.gl_set_swap_interval 1 |> sdl_u_; (* v-sync *)
    if get_gl_ctx gl_ctx |> is_null then (
      failwith "sdl init: gl_context is null";
    );

    (match Imgui_sys_opengl3.glewInit() with
       | Ok () -> ()
       | Error e -> Printf.printf "error in glew init: %s\n%!" e;
    );

    let ctx = I.igCreateContext None in
    let io = I.igGetIO () in

    if not @@ Ffi.init_for_opengl w (get_gl_ctx gl_ctx) then (
      failwith "failed during initialization of openGL SDL_Window";
    );

    if !log_ then Printf.eprintf "init imgui opengl\n%!";
    if not @@ Imgui_sys_opengl3.init None then (
      failwith "failed to initialize imgui<->openGL3"
    );
    if !log_ then Printf.eprintf "imgui opengl initialized\n%!";

    let open I.ImGuiIO in

    setf !@io f_DisplaySize (I.vec2 (float_of_int width) (float_of_int h));
    let fonts = getf !@io f_Fonts in
    let tex_w = allocate int 0 in
    let tex_h = allocate int 0 in
    let tex_pixels =
      allocate (ptr uchar) (coerce (ptr void) (ptr uchar) null) in
    let bytes_per_pixels = allocate int 0 in
    I.f_ImFontAtlas_GetTexDataAsRGBA32 (Some fonts)
      (Some tex_pixels) (Some tex_w) (Some tex_h) (Some bytes_per_pixels);
    I.igStyleColorsDark None;

    let sdl_event = Sdl.Event.create () in
    { w; sdl_w; gl_ctx; ctx; io; sdl_event; }

  let start_next_frame ?(on_quit=fun _ ->()) (self:t) : unit =
    (* poll and handle events *)
    if !log_ then Printf.eprintf "start next frame\n%!";
    while Sdl.poll_event (Some self.sdl_event) do
      (* NOTE: unsafe for now *)
      let ok =
        Imgui_sys_sdl2.process_event (Imgui_sys_sdl2.get_sdl_event self.sdl_event) in
      if not ok && !log_ then Printf.eprintf "imgui: fail to process event\n%!";
      if Sdl.Event.(get self.sdl_event typ) = Sdl.Event.quit then (
        on_quit ()
      ) else if Sdl.Event.(get self.sdl_event typ) = Sdl.Event.window_event &&
                Sdl.Event.(get self.sdl_event window_event_id) =
                Sdl.Event.window_event_close
                (* FIXME
                &&
                Sdl.Event.(get sdl_event window_window_id
                *)
      then (
        on_quit ()
      )
    done;
    if !log_ then Printf.eprintf "new frame\n%!";
    Imgui_sys_opengl3.new_frame();
    if !log_ then Printf.eprintf "new frame opengl ✔\n%!";
    Imgui_sys_sdl2.new_frame self.w;
    if !log_ then Printf.eprintf "new frame sdl ✔\n%!";
    I.igNewFrame();
    if !log_ then Printf.eprintf "new frame IG ✔\n%!";
    ()

  let render_frame (self:t) : unit =
    if !log_ then Printf.eprintf "render frame\n%!";
    I.igRender();
    let open I.ImGuiIO in
    Gl.viewport
      0 0
      (int_of_float @@ getf (getf !@ (self.io) f_DisplaySize) I.ImVec2.x)
      (int_of_float @@ getf (getf !@ (self.io) f_DisplaySize) I.ImVec2.y);
    Gl.clear Gl.color_buffer_bit;
    Imgui_sys_opengl3.render_data (I.igGetDrawData ());
    Sdl.gl_swap_window self.sdl_w;
    ()

  let shutdown (self:t) : unit =
    if !log_ then Printf.eprintf "shutdown…\n%!";
    Imgui_sys_opengl3.shutdown();
    Imgui_sys_sdl2.shutdown ();
    I.igDestroyContext (Some self.ctx);
    Sdl.destroy_window self.sdl_w;
    Sdl.quit();
    if !log_ then Printf.eprintf "bye!\n%!";
    ()

end

let clamp a b x = max a (min b x)

let main_loop ?w ?h ?flags ?on_quit ?(fps=40) ?(stop=fun _ -> false)
    f : unit =
  let fps = clamp 20 120 fps in
  let st = State.init ?flags ?w ?h () in
  try
    while not (stop ()) do
      let start = Unix.gettimeofday () in

      State.start_next_frame ?on_quit st;
      f();
      State.render_frame st;

      (* wait before doing the next frame *)
      let stop = Unix.gettimeofday() in
      let elapsed = stop -. start in
      let frame_duration = 1. /. (float_of_int fps) in
      if elapsed < frame_duration then (
        Unix.sleepf (frame_duration -. elapsed)
      );
    done;
    State.shutdown st
  with e ->
    State.shutdown st;
    raise e

