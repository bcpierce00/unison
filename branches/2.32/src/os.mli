(* Unison file synchronizer: src/os.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

val myCanonicalHostName : string

val tempPath : ?fresh:bool -> Fspath.t -> Path.local -> Path.local
val tempFilePrefix : string
val includeInTempNames : string -> unit

val exists : Fspath.t -> Path.local -> bool

val createUnisonDir : unit -> unit
val fileInUnisonDir : string -> Fspath.t
val unisonDir : Fspath.t

val childrenOf : Fspath.t -> Path.local -> Name.t list
val readLink : Fspath.t -> Path.local -> string
val symlink : Fspath.t -> Path.local -> string -> unit

val rename : string -> Fspath.t -> Path.local -> Fspath.t -> Path.local -> unit
val createDir : Fspath.t -> Path.local -> Props.t -> unit
val delete : Fspath.t -> Path.local -> unit

(* We define a new type of fingerprints here so that clients of
   Os.fingerprint do not need to worry about whether files have resource
   forks, or whatever, that need to be fingerprinted separately.  They can
   sensibly be compared for equality using =.  Internally, a fullfingerprint
   is a pair of the main file's fingerprint and the resource fork fingerprint,
   if any. *)
type fullfingerprint
val fullfingerprint_to_string : fullfingerprint -> string
val reasonForFingerprintMismatch : fullfingerprint -> fullfingerprint -> string
val fullfingerprint_dummy : fullfingerprint

(* Use this function if the file may change during fingerprinting *)
val safeFingerprint :
  Fspath.t -> Path.local -> (* coordinates of file to fingerprint *)
  Fileinfo.t ->             (* old fileinfo *)
  fullfingerprint option -> (* fingerprint corresponding to the old fileinfo *)
  Fileinfo.t * fullfingerprint
                            (* current fileinfo, fingerprint and fork info *)
val fingerprint :
  Fspath.t -> Path.local -> (* coordinates of file to fingerprint *)
  Fileinfo.t ->             (* old fileinfo *)
  fullfingerprint           (* current fingerprint *)

(* Versions of system calls that will restart when interrupted by
   signal handling *)
val accept : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr)

(* Called during program initialization to resolve a circular dependency
   between this module and Xferhints *)
val initializeXferFunctions : 
    (Fspath.t * Path.local -> unit) -> 
    ((Fspath.t * Path.local) -> (Fspath.t * Path.local) -> unit) ->
    unit

val quotes : string -> string
