(* Unison file synchronizer: src/fpcache.ml *)
(* Copyright 1999-2012, Benjamin C. Pierce

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

let debug = Trace.debug "fpcache"

(* In-memory cache *)

module PathTbl =
  Hashtbl.Make
    (struct
       type t = string
       let equal (s1 : string) (s2 : string) = s1 = s2
       let hash = Hashtbl.hash
     end)

let tbl = PathTbl.create 101

(* Information for writing to the on-disk cache *)

type entry =
  int * string * (Props.t * Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp)

type state =
  { oc : out_channel;
    mutable count : int;
    mutable size : Uutil.Filesize.t;
    mutable last : string;
    mutable queue : entry list }

let state = ref None

(****)

(* Path compression and decompression (use delta from previous path for
   compression) *)

let decompress st i path =
  let l = String.length path in
  let s = String.create (l + i) in
  String.blit !st 0 s 0 i;
  String.blit path 0 s i l;
  st := s;
  s

let compress state path =
  let s = state.last in
  let p = Path.toString path in
  let l = min (String.length p) (String.length s) in
  let i = ref 0 in
  while !i < l && p.[!i] = s.[!i] do incr i done;
  state.last <- p;
  (!i, String.sub p !i (String.length p - !i))

(*****)

(* Read and write a chunk of file fingerprints from the cache *)

let read st ic =
  (* I/O errors are dealt with at a higher level *)
  let fp1 = Digest.input ic in
  let fp2 = Digest.input ic in
  let headerSize = Marshal.header_size in
  let header = String.create headerSize in
  really_input ic header 0 headerSize;
  if fp1 <> Digest.string header then begin
    debug (fun () -> Util.msg "bad header checksum\n");
    raise End_of_file
  end;
  let dataSize = Marshal.data_size header 0 in
  let s = String.create (headerSize + dataSize) in
  String.blit header 0 s 0 headerSize;
  really_input ic s headerSize dataSize;
  if fp2 <> Digest.string s then begin
    debug (fun () -> Util.msg "bad chunk checksum\n");
    raise End_of_file
  end;
  let q : entry list = Marshal.from_string s 0 in
  debug (fun () -> Util.msg "read chunk of %d files\n" (List.length q));
  List.iter (fun (l, p, i) -> PathTbl.add tbl (decompress st l p) i) q

let closeOut st =
  state := None;
  try
    close_out st.oc
  with Sys_error error ->
    debug (fun () -> Util.msg "error in closing cache file: %s\n" error)

let write state =
  let q = Safelist.rev state.queue in
  let s = Marshal.to_string q [Marshal.No_sharing] in
  let fp1 = Digest.substring s 0 Marshal.header_size in
  let fp2 = Digest.string s in
  begin try
    Digest.output state.oc fp1; Digest.output state.oc fp2;
    output_string state.oc s; flush state.oc
  with Sys_error error ->
    debug (fun () -> Util.msg "error in writing to cache file: %s\n" error);
    closeOut state
  end;
  state.count <- 0;
  state.size <- Uutil.Filesize.zero;
  state.queue <- []

(****)

(* Start and finish dealing with the cache *)

let finish () =
  PathTbl.clear tbl;
  match !state with
    Some st -> if st.queue <> [] then write st;
               closeOut st
  | None    -> ()

let magic = "Unison fingerprint cache format 2"

let init fastCheck ignorearchives fspath =
  finish ();
  if fastCheck && not ignorearchives then begin
    begin try
      debug (fun () -> Util.msg "opening cache file %s for input\n"
                         (System.fspathToDebugString fspath));
      let ic = System.open_in_bin fspath in
      begin try
        let header = input_line ic in
        if header <> magic then raise (Sys_error "wrong header");
        let st = ref "" in
        while true do read st ic done
      with
        Sys_error error ->
          debug (fun () -> Util.msg "error in loading cache file %s: %s\n"
                             (System.fspathToDebugString fspath) error)
      | End_of_file ->
          ()
      end;
      begin try
        close_in ic
      with Sys_error error ->
        debug (fun () -> Util.msg "error in closing cache file %s: %s\n"
                             (System.fspathToDebugString fspath) error)
      end;
    with Sys_error error ->
      debug (fun () -> Util.msg "could not open cache file %s: %s\n"
                         (System.fspathToDebugString fspath) error)
    end;
    begin try
      debug (fun () -> Util.msg "opening cache file %s for output\n"
                         (System.fspathToDebugString fspath));
      let oc =
        System.open_out_gen
          [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 fspath in
      output_string oc magic; output_string oc "\n"; flush oc;
      state :=
        Some { oc = oc; count = 0; size = Uutil.Filesize.zero;
               last = ""; queue = [] }
    with Sys_error error ->
      debug (fun () -> Util.msg "could not open cache file %s: %s\n"
                         (System.fspathToDebugString fspath) error)
    end
  end

(****)

(* Enqueue a fingerprint to be written to disk. *)

let maxCount = 5000
let maxSize = Uutil.Filesize.ofInt (100 * 1024 * 1024)

let save path v =
  match !state with
    None ->
      ()
  | Some state ->
      let (desc, _, _, _) = v in
      let l = Props.length desc in
      state.size <- Uutil.Filesize.add state.size l;
      state.count <- state.count + 1;
      let (l, s) = compress state path in
      state.queue <- (l, s, v) :: state.queue;
      if state.count > maxCount || state.size > maxSize then write state

(****)

(* Check whether a fingerprint is in the in-memory cache and store it
   to the on-disk cache in any case. *)

(* HACK: we disable fastcheck for Excel (and MPP) files, as Excel
   sometimes modifies a file without updating the time stamp. *)
let excelFile path =
  let s = Path.toString path in
     Util.endswith s ".xls"
  || Util.endswith s ".mpp"

let dataClearlyUnchanged fastCheck path info desc stamp =
  fastCheck
    &&
  Props.same_time info.Fileinfo.desc desc
    &&
  Props.length info.Fileinfo.desc = Props.length desc
    &&
  not (excelFile path)
    &&
  match stamp with
    Fileinfo.InodeStamp inode ->
      info.Fileinfo.inode = inode
  | Fileinfo.CtimeStamp ctime ->
      (* BCP [Apr 07]: This doesn't work -- ctimes are unreliable
                       under windows.  :-(
         info.Fileinfo.ctime = ctime *)
      true

let ressClearlyUnchanged fastCheck info ress dataClearlyUnchanged =
  fastCheck
    &&
  Osx.ressUnchanged ress info.Fileinfo.osX.Osx.ressInfo
    None dataClearlyUnchanged

let clearlyUnchanged fastCheck path newInfo oldDesc oldStamp oldRess =
  let du =
    dataClearlyUnchanged fastCheck path newInfo oldDesc oldStamp
  in
  du && ressClearlyUnchanged fastCheck newInfo oldRess du

let fastercheckUNSAFE =
  Prefs.createBool "fastercheckUNSAFE"
    false "!skip computing fingerprints for new files (experts only!)"
    (  "THIS FEATURE IS STILL EXPERIMENTAL AND SHOULD BE USED WITH EXTREME CAUTION.  "
       ^ "\n\n"
       ^ "When this flag is set to {\\tt true}, Unison will compute a 'pseudo-" 
       ^ "fingerprint' the first time it sees a file (either because the file is "
       ^ "new or because Unison is running for the first time).  This enormously "
       ^ "speeds update detection, but it must be used with care, as it can cause "
       ^ "Unison to miss conflicts: If "
       ^ "a given path in the filesystem contains files on {\\em both} sides that "
       ^ "Unison has not yet seen, and if those files have the same length but different "
       ^ "contents, then Unison will not notice the presence of a conflict.  If, later, one "
       ^ "of the files is changed, the changed file will be propagated, overwriting  "
       ^ "the other.  "
       ^ "\n\n"
       ^ "Moreover, even when the files are initially identical, setting this flag can lead "
       ^ "to potentially confusing behavior: "
       ^ "if a newly created file is later touched without being modified, Unison will "
       ^ "treat this "
       ^ "conservatively as a potential change (since it has no record of the earlier "
       ^ "contents) and show it as needing to be propagated to the other replica. "
       ^ "\n\n"
       ^ "Most users should leave this flag off -- the small time savings of not "
       ^ "fingerprinting new files is not worth the cost in terms of safety.  However, "
       ^ "it can be very useful for power users with huge replicas that are known to "
       ^ "be already synchronized (e.g., because one replica is a newly created duplicate "
       ^ "of the other, or because they have previously been synchronized with Unison but "
       ^ "Unison's archives need to be rebuilt).  In such situations, it is recommended "
       ^ "that this flag be set only for the initial run of Unison, so that new archives "
       ^ "can be created quickly, and then turned off for normal use.")

let fingerprint ?(newfile=false) fastCheck currfspath path info optFp =
  let res =
    try
      let (cachedDesc, cachedFp, cachedStamp, cachedRess) =
        PathTbl.find tbl (Path.toString path) in
      if
        not (clearlyUnchanged
               fastCheck path info cachedDesc cachedStamp cachedRess)
      then
        raise Not_found;
      debug (fun () -> Util.msg "cache hit for path %s\n"
                         (Path.toDebugString path));
      (info.Fileinfo.desc, cachedFp, Fileinfo.stamp info,
       Fileinfo.ressStamp info)
    with Not_found ->
      if fastCheck then
        debug (fun () -> Util.msg "cache miss for path %s\n"
                           (Path.toDebugString path));
      let (info, dig) =
        if Prefs.read fastercheckUNSAFE && newfile then begin
          debug (fun()-> Util.msg "skipping initial fingerprint of %s\n"
                            (Fspath.toDebugString (Fspath.concat currfspath path)));
          (Fileinfo.get false currfspath path,
           Os.pseudoFingerprint path (Props.length info.Fileinfo.desc))
        end else begin
          Os.safeFingerprint currfspath path info optFp
        end in
      (info.Fileinfo.desc, dig, Fileinfo.stamp info, Fileinfo.ressStamp info)
  in
  save path res;
  res
