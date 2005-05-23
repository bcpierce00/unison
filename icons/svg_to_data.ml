
(*
   Create a bitmap from a svg file, containing raw data suitable to be
   loaded using GdkPixbuf.from_data.

   Usage:
    lablgtk2 svg_data.ml U.svg > /tmp/U.data
*)

let buf = Rsvg.render_from_file Sys.argv.(1)
let _ =
Format.printf "\"%s\"@."
  (String.escaped (Gpointer.string_of_region (GdkPixbuf.get_pixels buf)))
