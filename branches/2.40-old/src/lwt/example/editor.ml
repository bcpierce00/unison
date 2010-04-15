let _ =
  let editor = try Sys.getenv "EDITOR" with Not_found -> "emacs" in
  Lwt_unix.run (Lwt_unix.system editor)
