
val start : string -> Fspath.t -> bool
val running : string -> bool
val getChanges : string -> Path.t list
val wait : string -> unit Lwt.t
