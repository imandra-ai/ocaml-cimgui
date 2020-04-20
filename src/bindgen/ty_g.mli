(** {1 graph of types, in dependency order} *)

type dep =
  | Dep_decl of string
  | Dep_def of string

type t
type ml_names = {decl: string; def: string option}
val create : tydefs:(string*string) list -> unit -> t
val find_ml_names : t -> string -> ml_names option
val add_decl : ?enum:bool -> t -> string -> ml_name:string -> code:(string*dep list) lazy_t-> unit
val add_def : t -> string -> ml_name:string -> code:(string*dep list) lazy_t -> unit
val sorted : t -> string list (* sorted in dependency order *)

val parse_ty :
  ?funptr:string -> ?ptr_top:string -> ?array_to_ptr:bool ->
  t -> string -> string * dep list
(** Translate C type to a [ctypes] expression
    @param funptr Name of ctypes function to use to represent function pointers
    @param ptr_top Name of ctypes function to use to represent the toplevel pointer
      (default "ptr")
*)

val def_unions : string

val to_file : t -> string -> unit
val of_file : string -> t
