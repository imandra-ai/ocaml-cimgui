
module Ctypes = Ctypes
module I = Imgui_sys
include Imgui_sys
include Ctypes

module Infix = struct
  let (!@) = (!@)
  (** Dereference the pointer *)

  let (<-@) = (<-@)
  (** Set the pointer's content *)

  let (@.) = (@.)
  (** Access a structure field *)
end
include Infix

let vec2 x y : ImVec2.t =
  let v2 = make ImVec2.t in
  setf v2 ImVec2.x x;
  setf v2 ImVec2.y y;
  v2

let vec4 x y z w : ImVec4.t =
  let v = make ImVec4.t in
  setf v ImVec4.x x;
  setf v ImVec4.y y;
  setf v ImVec4.z z;
  setf v ImVec4.w w;
  v

(** Create main menu bar, calls [f()] if it's enabled. *)
let main_menu_bar f =
  if I.igBeginMainMenuBar () then (
    f ();
    I.igEndMainMenuBar();
  )

(** Create menu list, calls [f()] if it's enabled. *)
let menu (label:string) (active:bool) f : unit =
  if I.igBeginMenu label active then (
    f();
    I.igEndMenu();
  )

let menu_item_bool ?(shortcut="") (label:string) f : unit =
  if I.igMenuItemBool label shortcut false true then f ()

(** Create a button, call [f()] if it's clicked *)
let button ?dim (label:string) f =
  let dim = match dim with
    | None -> vec2 (float_of_int @@ String.length label * 18) 18.
    | Some d -> d
  in
  if I.igButton label dim then (
    f()
  )

class text_input_callback_data (d: ImGuiInputTextCallbackData.t ptr) =
  let open ImGuiInputTextCallbackData in
  object(self)
    method event_flag = !@d @. f_EventFlag
    method flags = !@d @. f_Flags
    method user_data: [`Not_provided] = `Not_provided (* TODO? *)
    method event_char = !@d @. f_EventChar
    method event_key = !@d @. f_EventKey
    method buf = !@d @. f_Buf
    method buf_text_len = !@d @. f_BufTextLen
    method buf_size = !@d @. f_BufSize
    method buf_dirty = !@d @. f_BufDirty
    method cursor_pos = !@d @. f_CursorPos
    method selection_start = !@d @. f_SelectionStart
    method selection_end = !@d @. f_SelectionEnd

    method content =
      string_from_ptr (!@ (self#buf)) ~length:(!@ (self#buf_text_len))
  end

(** A text input widget *)
class text_input ?(bufsize=1024) ?(label="") ?flags () =
  let open ImGuiInputTextFlags in
  let flags = match flags with
    | None ->
      enterReturnsTrue lor alwaysInsertMode lor
      callbackCompletion lor callbackHistory
    | Some f -> f
  in
  object
    val input_buf = allocate_n char ~count:bufsize
    method reset = input_buf <-@ '\x00'
    method text =
      let s = string_from_ptr input_buf ~length:1024 in
      let s =
        try String.sub s 0 (String.index s '\x00')
        with Not_found -> s
      in
      s
    method render ?cb f =
      if igInputText label (Some input_buf) (Unsigned.Size_t.of_int bufsize)
          flags
          (fun acts -> match cb with
             | None -> 0
             | Some f -> f (new text_input_callback_data acts))
          null
      then (
        f ()
      )
  end

