
module Backend = Imgui_sdl_opengl3
module I = Imgui
open I.Infix

let main () =
  let quit = ref false in
  let i = ref 0 in
  let my_f = I.allocate I.Ctypes.float 0. in
  let simple_win = I.allocate I.Ctypes.bool false in
  let demo_win = I.allocate I.Ctypes.bool false in
  let on_frame () =
    incr i;
    I.igText1 (Printf.sprintf "hello world! (frame %d, float %.3f) %%s" !i !@my_f) "howdy";
    let slider_changed =
      I.igSliderFloat "float slider" (Some my_f) 0. 1. "%.3f" 1.0
    in
    if slider_changed then Printf.printf "slider touched! val=%.3f\n%!" !@my_f;
    if I.igCheckbox "show demo window" (Some demo_win) then (
      Printf.printf "checked boxed for demo window";
    );
    if I.igButton "show other window" (I.vec2 400. 400.) then (
      Printf.printf "display other window";
      simple_win <-@ true;
    );
    if !@ demo_win then (
      I.igShowDemoWindow (Some demo_win);
    );

    if !@ simple_win then (
      ignore (I.igBegin "simple window" None 0 : bool);
      I.igText "tada!";
      if I.igButton "close me" (I.vec2 60. 60.) then simple_win <-@ false;
      I.igEnd();
    );
  in
  Backend.main_loop
    ~on_quit:(fun () -> quit := true)
    ~stop:(fun () -> !quit)
    ~fps:50
    on_frame

let () =
  let opts = [
    "-d", Arg.Set Backend.log_, " enable logging";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> ()) "usage: example";
  main()
