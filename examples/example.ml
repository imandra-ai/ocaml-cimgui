open Ctypes
module Sdl = Tsdl.Sdl

module I = Imgui_sys

let sdl_u_ = function
  | Ok x -> x
  | Error (`Msg e) -> failwith @@ "sdl error " ^ e

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
