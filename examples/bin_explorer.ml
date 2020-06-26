
module Backend = Imgui_sdl_opengl3
module I = Imgui
open I.Infix

module M = Msgpck

let rec explore_msgpack (m:M.t) : unit =
  let spf = Printf.sprintf in
  match m with
  | M.Nil -> I.igText "nil"
  | M.Bool b -> I.igText @@ spf "%B" b
  | M.Float f -> I.igText @@ spf "%f" f
  | M.Float32 f -> I.igText @@ spf "%f" @@ Int32.float_of_bits f
  | M.Int i -> I.igText @@ spf "%d" i
  | M.Uint32 i -> I.igText @@ spf "%ld" i
  | M.Int32 i -> I.igText @@ spf "%ld" i
  | M.Uint64 i -> I.igText @@ spf "%Ld" i
  | M.Int64 i -> I.igText @@ spf "%Ld" i
  | M.List l ->
    if I.igCollapsingHeaderBoolPtr "list" None 0 then (
      List.iteri (fun i x ->
          I.igPushIDInt i;
          I.igBullet();
          I.igTreePushStr "t";
          explore_msgpack x;
          I.igTreePop();
          I.igPopID())
        l;
    )
  | M.Map l ->
    if I.igCollapsingHeaderBoolPtr "map" None 0 then (
      List.iteri (fun i (x,y) ->
          I.igNewLine();
          I.igPushIDInt i;
          I.igSameLine 0. (-1.);
          I.igBullet();
          explore_msgpack x;
          I.igNewLine();
          explore_msgpack y;
          I.igPopID())
        l;
    )
  | M.String s -> I.igText s
  | M.Bytes s ->
    if CCUtf8_string.is_valid s then I.igText s
    else I.igText (spf "<bytes %d>" (String.length s))
  | M.Ext (i,s) -> I.igText @@ spf "ext(%d): %s" i s

type st =
  | St_nil
  | St_msgpack of M.t list

let main (files:string list) =
  let quit = ref false in
  let files = ref files in
  let st = ref St_nil in
  let on_frame () =
    if I.igBegin "explorer" None I.ImGuiWindowFlags.(alwaysUseWindowPadding lor none) then (
      begin match !st, !files with
        | St_nil, [] -> I.igText "<no file>"
        | St_nil, f :: fs ->
          files := fs;
          (match CCIO.with_in f
                   (fun ic -> snd @@ M.StringBuf.read_all (CCIO.read_all ic))
           with
           | l -> st := St_msgpack l
           | exception _ -> () (* TODO: error handling *)
          )
        | St_msgpack l, _ ->
          explore_msgpack (M.of_list l)
      end;
    );
    I.igEnd();
  in
  Backend.main_loop
    ~on_quit:(fun () -> quit := true)
    ~stop:(fun () -> !quit)
    ~fps:50
    on_frame

let () =
  let files = ref [] in
  let opts = [
    "-d", Arg.Set Backend.log_, " enable logging";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "usage: explore_bin file+";
  main @@ List.rev !files
