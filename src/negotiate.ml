(* Unison file synchronizer: src/negotiate.ml *)
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

let (>>=) = Lwt.bind

let debug = Util.debug "features"

let debugFeatures name features =
  debug (fun () ->
    Util.msg "%s:\n" name;
    Safelist.iter (fun n -> Util.msg "  - %s\n" n) features)

let getCommonFeaturesLocal (root, features) =
  Features.resetEnabled ();
  let supportedFeatures = Features.all () in
  debugFeatures "Supported features" supportedFeatures;
  debugFeatures "Received features for feature negotiation" features;
  let common = Features.inter features supportedFeatures in
  debugFeatures "Selected common features" common;
  try
    let () = Features.validate common in
    let () = Features.setEnabled common in
    Lwt.return common
  with
  | e -> Lwt.fail e

let m = Umarshal.(list string)

let negotiateFeaturesRpcName = "negotiateFeatures"
let getCommonFeaturesRemote =
  Remote.registerRootCmd negotiateFeaturesRpcName m m getCommonFeaturesLocal

let getCommonFeaturesOnRoot features = function
  | (Common.Local, _) -> Lwt.return features
  | root -> getCommonFeaturesRemote root features

let commonFeatures root fts =
  getCommonFeaturesOnRoot fts root >>= fun common ->
  let rn = "Common features for root " ^ Common.root2string root in
  debugFeatures rn common;
  try
    let () = Features.validate common in
    Lwt.return common
  with
  | e -> Lwt.fail e

let allRootsSupportFeatures roots =
  let aux k r =
    let supp = Remote.commandAvailable r negotiateFeaturesRpcName in
    k >>= fun k' ->
    supp >>= fun supp' ->
    Lwt.return (k' && supp')
  in
  Safelist.fold_left aux (Lwt.return true) roots

let features roots =
  Features.resetEnabled ();
  let supportedFeatures = Features.all () in
  debugFeatures "Supported features" supportedFeatures;
  allRootsSupportFeatures roots >>= (fun supported ->
  if not supported then begin
    debug (fun () -> Util.msg "The server does not support \"features\".\n");
    Lwt.return (Features.empty)
  end else
    Safelist.fold_left (fun fts r -> fts >>= commonFeatures r)
      (Lwt.return supportedFeatures) roots
  ) >>= fun common ->
  debugFeatures "Enabled features" common;
  Lwt.return (Features.setEnabled common)

