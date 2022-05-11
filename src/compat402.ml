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
