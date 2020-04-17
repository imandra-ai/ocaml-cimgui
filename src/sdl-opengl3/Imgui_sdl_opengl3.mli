(** {1 High-level wrapper for running Imgui in openGL3 + SDL} *)

module State : sig
  type t

  val init :
    ?w:int -> ?h:int -> ?flags:Tsdl.Sdl.Window.flags ->
    unit -> t

  val start_next_frame :
    ?on_quit:(unit -> unit) ->
    t -> unit
  (** @param on_quit called if the SDL events indicate the application
      should quit *)

  val render_frame : t -> unit

  val shutdown : t -> unit
end

val main_loop :
  ?w:int -> ?h:int -> ?flags:Tsdl.Sdl.Window.flags ->
  ?on_quit:(unit -> unit) ->
  ?fps:int -> ?stop:(unit->bool) ->
  (unit -> unit) -> unit

(** [main_loop f] initializes imgui, run [f ()] within each frame.
    @param fps number of frames per second.
    @param stop called before each frame to see if we should stop
    @param on_quit called if SDL events indicate we should quit the application
*)

(**/**)
val log_ : bool ref
(**/**)
