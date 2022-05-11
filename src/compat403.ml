(* Functions added in OCaml 4.03 *)

module String = struct
  include String

  let lowercase_ascii = lowercase
  let capitalize_ascii = capitalize
end

module Unix = struct
  include Unix

  let has_symlink () = not Sys.win32
end

module Sys = struct
  include Sys

  let int_size = word_size - 1
end
