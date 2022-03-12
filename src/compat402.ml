(* Functions added in OCaml 4.02 *)

type bytes = string

let output_bytes = output_string
let output_substring = output

module Bytes = struct
  include String

  let of_string x = x
  let to_string x = x

  let sub_string = sub
end

module Buffer = struct
  include Buffer

  let add_subbytes = add_substring
end

module Digest = struct
  include Digest

  let bytes = string
  let subbytes = substring
end

module Marshal = struct
  include Marshal

  let from_bytes = from_string
end

module Unix = struct
  include Unix

  let write_substring = write
end

module Format = struct
  include Format

  (* Copied from OCaml source *)
  let pp_print_text ppf s =
    let len = String.length s in
    let left = ref 0 in
    let right = ref 0 in
    let flush () =
      pp_print_string ppf (String.sub s !left (!right - !left));
      incr right; left := !right;
    in
    while (!right <> len) do
      match s.[!right] with
        | '\n' ->
          flush ();
          pp_force_newline ppf ()
        | ' ' ->
          flush (); pp_print_space ppf ()
        (* there is no specific support for '\t'
           as it is unclear what a right semantics would be *)
        | _ -> incr right
    done;
    if !left <> len then flush ()
end
