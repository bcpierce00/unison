(* Functions added in OCaml 4.03 *)

type ('a,'b) result = Ok of 'a | Error of 'b

module String = struct
  include String

  let lowercase_ascii = lowercase
  let capitalize_ascii = capitalize
  let uncapitalize_ascii = uncapitalize
end

module Unix = struct
  include Unix

  let has_symlink () = not Sys.win32
end

module Sys = struct
  include Sys

  let int_size = word_size - 1
end
