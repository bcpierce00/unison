(* Unison file synchronizer: src/fswatcherold.ml *)
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

(* FIX: we should check that the child process has not died and
   restart it if so... *)

(* FIX: the names of the paths being watched should get included
   in the name of the watcher's state file *)

let debug = Util.debug "fswatch"

let watchinterval = 5

let watcherTemp archHash n = Os.fileInUnisonDir (n ^ archHash)

let watchercmd archHash root =
  let fsmonfile =
    Filename.concat (Filename.dirname Sys.executable_name) "fsmonitor.py" in
  if not (Sys.file_exists fsmonfile) then
    None
  else begin
    (* FIX: is the quoting of --follow parameters going to work on Win32?
         (2/2012: tried adding Uutil.quotes -- maybe this is OK now?) *)
    (* FIX -- need to find the program using watcherosx preference *)
    let changefile = watcherTemp archHash "changes" in
    let statefile = watcherTemp archHash "state" in
    let paths = Safelist.map Path.toString (Prefs.read Globals.paths) in
    let followpaths = Pred.extern Path.followPred in
    let follow = Safelist.map
                   (fun s -> "--follow '" ^ Uutil.quotes s ^ "'")
                   followpaths in
  (* BCP (per Josh Berdine, 5/2012): changed startup command from this...
       let cmd = Printf.sprintf "fsmonitor.py %s --outfile %s --statefile %s %s %s\n"
     ... to this: *)
    let cmd = Printf.sprintf "python \"%s\" \"%s\" --outfile \"%s\" --statefile \"%s\" %s %s\n"
                fsmonfile
                root
                (System.fspathToPrintString changefile)
                (System.fspathToPrintString statefile)
                (String.concat " " follow)
                (String.concat " " paths) in
    debug (fun() -> Util.msg "watchercmd = %s\n" cmd);
    Some (changefile,cmd)
  end

module StringSet= Set.Make (String)
module RootMap = Map.Make (String)
type watcherinfo = {file: System.fspath;
                    mutable ch:Pervasives.in_channel option;
                    chars: Buffer.t;
                    mutable lines: string list}
let watchers : watcherinfo RootMap.t ref = ref RootMap.empty
let newWatchers = ref StringSet.empty

let trim_duplicates l =
  let rec loop l = match l with
    [] -> l
  | [s] -> l
  | s1::s2::rest ->
      if Util.startswith s1 s2 || Util.startswith s2 s1 then
        loop (s2::rest)
      else
        s1 :: (loop (s2::rest)) in
  loop (Safelist.sort String.compare l)  

let readAvailableLinesFromWatcher wi =
  let ch = match wi.ch with Some(c) -> c | None -> assert false in 
  let rec loop () =
    match try Some(input_char ch) with End_of_file -> None with
      None ->
        ()
    | Some(c) ->
        if c = '\n' then begin
          wi.lines <- Buffer.contents wi.chars :: wi.lines;
          Buffer.clear wi.chars;
          loop ()
        end else begin
          Buffer.add_char wi.chars c;
          loop ()
        end in
    loop ()

let readChanges wi =
  if wi.ch = None then
    (* Watcher channel not built yet *)
    if System.file_exists wi.file then begin
      (* Build it and go *)
      let c = System.open_in_bin wi.file in
      wi.ch <- Some c;
      readAvailableLinesFromWatcher wi;
    end else begin
      (* Wait for change file to be built *)
      debug (fun() -> Util.msg
        "Waiting for change file %s\n"
        (System.fspathToPrintString wi.file))
    end
  else
    (* Watcher running and channel built: go ahead and read *)
    readAvailableLinesFromWatcher wi

let getChanges archHash =
  if StringSet.mem archHash !newWatchers then
    Fswatch.getChanges archHash
  else begin
    let wi = RootMap.find archHash !watchers in
    readChanges wi;
    let res = wi.lines in
    wi.lines <- [];
    List.map Path.fromString (trim_duplicates res)
  end

let start archHash fspath =
  if not (Prefs.read Fswatch.useWatcher) then
    false
  else if Fswatch.start archHash then begin
    newWatchers := StringSet.add archHash !newWatchers;
    true
  end else if not (RootMap.mem archHash !watchers) then begin
    (* Watcher process not running *)
    match watchercmd archHash (Fspath.toString fspath) with
      Some (changefile,cmd) ->
        debug (fun() -> Util.msg
                 "Starting watcher on fspath %s\n"
                 (Fspath.toDebugString fspath));
        let _ = System.open_process_out cmd in
        let wi = {file = changefile; ch = None;
                  lines = []; chars = Buffer.create 80} in
        watchers := RootMap.add archHash wi !watchers;
        true
    | None ->
        false
  end else begin
    (* If already running, discard all pending changes *)
    ignore (getChanges archHash);
    true
  end

let wait archHash =
  if StringSet.mem archHash !newWatchers then
    Fswatch.wait archHash
  else if not (RootMap.mem archHash !watchers) then
    raise (Util.Fatal "No file monitoring helper program found")
  else begin
    let wi = RootMap.find archHash !watchers in
    let rec loop () =
      readChanges wi;
      if wi.lines = [] then begin
        debug (fun() -> Util.msg "Sleeping for %d seconds...\n" watchinterval);
        Lwt.bind (Lwt_unix.sleep (float watchinterval)) (fun () ->
        loop ())
      end else
        Lwt.return ()
    in
    loop ()
  end
