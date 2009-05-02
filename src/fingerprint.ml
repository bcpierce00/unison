(* Unison file synchronizer: src/fingerprint.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

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


(* NOTE: IF YOU CHANGE TYPE "FINGERPRINT", THE ARCHIVE FORMAT CHANGES;       *)
(* INCREMENT "UPDATE.ARCHIVEFORMAT"                                          *)
type t = string

(* Assumes that (fspath, path) is a file and gives its ``digest '', that is  *)
(* a short string of cryptographic quality representing it.                  *)
let file fspath path =
  let f = Fspath.toString (Fspath.concat fspath path) in
  Util.convertUnixErrorsToTransient
    ("digesting " ^ f)
    (fun () -> Digest.file f)

let maxLength = Uutil.Filesize.ofInt max_int
let subfile path offset len =
  if len > maxLength then
    raise (Util.Transient
             (Format.sprintf "File '%s' too big for fingerprinting" path));
  Util.convertUnixErrorsToTransient
    "digesting subfile"
    (fun () ->
       let inch = open_in_bin path in
       begin try
         LargeFile.seek_in inch offset;
         let res = Digest.channel inch (Uutil.Filesize.toInt len) in
         close_in inch;
         res
       with
         End_of_file ->
           close_in_noerr inch;
           raise (Util.Transient
                    (Format.sprintf
                       "Error in digesting subfile '%s': truncated file" path))
       | e ->
           close_in_noerr inch;
           raise e
       end)

let int2hexa quartet =
  if quartet < 10 then
    (char_of_int ((int_of_char '0') + quartet))
  else char_of_int ((int_of_char 'a') + quartet - 10)

let hexaCode theChar =
  let intCode = int_of_char theChar in
  let first = intCode / 16 in
  let second = intCode mod 16 in
  (int2hexa first, int2hexa second)

let toString md5 =
  let length = String.length md5 in
  let string = String.create (length * 2) in
  for i=0 to (length - 1) do
    let c1, c2 =  hexaCode (md5.[i]) in
    string.[2*i] <- c1;
    string.[2*i + 1] <- c2;
  done;
  string

let string = Digest.string

let dummy = ""
