(* Unison file synchronizer: src/moves.mli *)
(* Copyright 2021-2023, Tõivo Leedjärv

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

val enabled : unit -> bool
(** Is the moves feature enabled? *)

val reset : unit -> unit
(** Clear all move candidates and detected moves from persistent state. *)

val addHint : Path.t -> Common.replicas -> Common.replicas
(** [addHint path rplc] considers whether [rplc] could be a move source or
    a target and if so, records it as a move candidate for [path].
    Always returns unchanged [rplc]. *)

val detect : unit -> unit
(** Detects moves/renames from move candidates hinted by [addHint] and
    prepares difference records for moved paths. *)

val r : Common.reconItem -> Common.reconItem option
(** [r ri] returns a move-reconItem for [ri.path] or the unchanged [ri] if
    no move was detected on the path. Returns [None] if the reconItem must
    be pruned from reconciliation results (it is shadowed by a move). *)

val originalRc : Common.replicaContent -> Common.replicaContent
(** [originalRC rc] returns the original non-move replicaContent if [rc] is
    a move replicaContent, or unchanged [rc] otherwise. *)
