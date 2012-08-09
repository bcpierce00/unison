
val debug : bool ref

val error : string -> 'a
val format_exc : exn -> string

module StringMap : Map.S with type key = string

module F (M : sig type watch end) : sig

  type t

  val get_id : t -> int
  val get_watch : t -> M.watch option
  val set_watch : t -> M.watch option -> unit
  val get_subdirs : t -> t StringMap.t
  val is_root : t -> bool

  val file_by_id : (int, t) Hashtbl.t
  val dir_path : t -> string -> string

  val signal_change :
    float ref -> t -> string option -> [> `CREAT | `DEL ] -> unit
  val signal_overflow : unit -> unit

  module type S = sig
    val add_watch : string -> t -> unit
    val release_watch : t -> unit
    val watch : unit -> unit
    val clear_event_memory : unit -> unit
  end

  module F (M :S) : sig end

end
