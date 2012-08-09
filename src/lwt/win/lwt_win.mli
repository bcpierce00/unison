type notify_filter_flag =
    FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME
  | FILE_NOTIFY_CHANGE_ATTRIBUTES | FILE_NOTIFY_CHANGE_SIZE
  | FILE_NOTIFY_CHANGE_LAST_WRITE | FILE_NOTIFY_CHANGE_LAST_ACCESS
  | FILE_NOTIFY_CHANGE_CREATION | FILE_NOTIFY_CHANGE_SECURITY

type file_action =
    FILE_ACTION_ADDED | FILE_ACTION_REMOVED
  | FILE_ACTION_MODIFIED | FILE_ACTION_RENAMED_OLD_NAME
  | FILE_ACTION_RENAMED_NEW_NAME

type directory_handle

(* Returns an empty list in case of overflow. *)
val readdirectorychanges :
  directory_handle -> bool -> notify_filter_flag list ->
  (string * file_action) list Lwt.t

val open_directory : string -> directory_handle
val close_dir : directory_handle -> unit
