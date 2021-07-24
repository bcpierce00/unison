(* Unison file synchronizer: src/features.mli *)
(* Copyright 2021, Tõivo Leedjärv

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

type t
(** The type of a feature. *)

type id = string
(** The type of feature's identifier. Features are identified by their name. *)

val enabled : t -> bool
(** Test whether a feature is currently enabled (included in the current
    set of enabled features).

    Feature negotiation must have been completed to get the correct result. *)

val register : string -> ?arcFormatChange:bool ->
        (id list -> bool -> string option) option -> t
(** [register n f] registers a supported feature with a unique identifier [n].

    [f] is an optional validation function that will be called during feature
    negotiation. [f] will receive as the first argument the feature set to be
    enabled as a result of negotiation and as the second argument a boolean
    indicating whether the tested feature is included in the negotiated set.
    [f] must return [Some msg] if the negotiation result must be rejected with
    the error message [msg], otherwise it must return [None].

    [archFormatChange] is an optional argument which indicates whether the
    feature, if enabled, changes the archive format that is stored on disk.
    In other words, it indicates if the archive stored while this feature was
    enabled requires the existence of this feature to be read back in.

    @return feature value that can be tested by {!Features.enabled} function.
    @raise {!Util.Fatal} if [n] is not unique. *)

val dummy : t
(** A feature value that will never be included in feature negotiation or
    the set of enabled features. *)

val all : unit -> id list
(** Set of all supported features registered by {!Features.register}. *)

val empty : id list
(** Empty set of features. *)

val changingArchiveFormat : unit -> id list
(** Set of all currently enabled features that impact the on-disk archive
    format. The same features must exist in order to read in the archive. *)

val mem : id -> id list -> bool
(** [mem n s] tests whether feature with id [n] belongs to feature set [s]. *)

val inter : id list -> id list -> id list
(** Feature set intersection. *)

val validate : id list -> unit
(** [validate s] calls validation functions associated with each registered
    feature in arbitrary order, with only features in [s] considered enabled.

    @raise {!Util.Fatal} at first failed validation. *)

val resetEnabled : unit -> unit
(** Make the set of enabled features empty. Can be used to reset the results
    of previous feature negotiation. *)

val setEnabled : id list -> unit
(** [setEnabled s] makes [s] the set of enabled features. *)

val validateEnabled : unit -> unit
(** Same as {!Features.validate} with the set of currently enabled features. *)

