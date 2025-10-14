module Digest = struct
  include Digest

  (* For OCaml < 5.2.0 *)
  module MD5 = struct
    include Digest
  end
end
