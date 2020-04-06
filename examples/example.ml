open Ctypes
module Sdl = Tsdl.Sdl

module I = Imgui_sys

let sdl_u_ = function
  | Ok x -> x
  | Error (`Msg e) -> failwith @@ "sdl error " ^ e

(*
// dear imgui: null/dummy example application
// (compile and link imgui, create context, run headless with NO INPUTS, NO GRAPHICS OUTPUT)
// This is useful to test building, but you cannot interact with anything here!
#include "imgui.h"
#include <stdio.h>

int main(int, char** )
{
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO();

    // Build atlas
    unsigned char* tex_pixels = NULL;
    int tex_w, tex_h;
    io.Fonts->GetTexDataAsRGBA32(&tex_pixels, &tex_w, &tex_h);

    for (int n = 0; n < 20; n++)
    {
        printf("NewFrame() %d\n", n);
        io.DisplaySize = ImVec2(1920, 1080);
        io.DeltaTime = 1.0f / 60.0f;
        ImGui::NewFrame();

        static float f = 0.0f;
        ImGui::Text("Hello, world!");
        ImGui::SliderFloat("float", &f, 0.0f, 1.0f);
        ImGui::Text("Application average %.3f ms/frame (%.1f FPS)", 1000.0f / io.Framerate, io.Framerate);
        ImGui::ShowDemoWindow(NULL);

        ImGui::Render();
    }

    printf("DestroyContext()\n");
    ImGui::DestroyContext();
    return 0;
}
*)

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
    ignore_bool @@ I.igSliderFloat "float slider" (Some my_f) 0. 1. "%.3f" 1.0;
(*     I.igShowDemoWindow *)


    I.igRender();
    ()
  done;

  I.igDestroyContext (Some _ctx);
  ()

(* TODO
let () =
(*
  let _w = Sdl.create_window ~w:800 ~h:600 "example"
      Sdl.Window.opengl |> sdl_u_ in
*)
(*   I.igStyleColorsDark; *)

  let active = allocate bool true in
  while I.igBegin "coucou" active 0
  (* TODO: coerce (Ctypes. I.ImGuiWindowFlags_.t int @@ I.ImGuiWindowFlags_.MenuBar) *)
  do
    let vec = make I.ImVec2.t in
    setf vec I.ImVec2.x 30.;
    setf vec I.ImVec2.y 14.;
    if I.igButton "coucou" vec then (
      print_endline "clicked";
    );
  done;
  ()
   *)
