(* Unison file synchronizer: src/monitoring-linux/lwt_inotify.mli *)
(* Copyright 2012, Benjamin C. Pierce (see COPYING for details) *)

type t

val init : unit -> t
val add_watch : t -> string -> Inotify.select_event list -> Inotify.wd
val rm_watch : t -> Inotify.wd -> unit
val read : t -> Inotify.event Lwt.t
val close : t -> unit (*Lwt.t*)
