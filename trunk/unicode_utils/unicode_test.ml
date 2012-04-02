(*-*-coding: utf-8;-*-*)

#use "unicode_build.ml"
#load "unix.cma"

let _ =
Unix.system "test -f unicode_tables.cmo || ocamlc -c unicode_tables.ml";;

#load "unicode_tables.cmo"
#use "unicode.ml"
#use "reorder.ml"

let _ =
  let b1 = Buffer.create 1024 in
  let b2 = Buffer.create 1024 in
  for i = 0 to 0x1ffff do
    if i < 0xd800 || i > 0xdfff then begin
      Buffer.add_string b1 (encode_utf8 i);
      Buffer.add_string b2 (conv i)
    end
  done;
  let s1 = Buffer.contents b1 in
  let s2 = Buffer.contents b2 in
prerr_endline "===";
Format.printf "%d %d@." (String.length s1) (String.length s2);
Format.printf "%d %d@." (String.length (normalize s1)) (String.length s2);
  assert (normalize s1 = s2);
  assert (normalize s2 = s2);
  assert (normalize (compose s2) = s2);
  assert (from_utf_16 (to_utf_16 s1) = s1)

let _ =
  let b1 = Buffer.create 1024 in
  let b2 = Buffer.create 1024 in
  for i = hangul_sbase -128 to hangul_sbase + hangul_scount - 1 + 128 do
    Buffer.add_string b1 (encode_utf8 i);
    Buffer.add_string b2 (conv i)
  done;
  let s1 = Buffer.contents b1 in
  let s2 = Buffer.contents b2 in
  assert (compose s2 = s1)

let _ =
  assert (compare "abcdéfgh" "ABCDÉFGH" = 0);
  assert (compare "abcdéfghi" "ABCDÉFGH" = 1);
  assert (compare "abcdefghi" "ABCDeFGH" = 1);
  assert (compare "abcdéfgh" "ABCDÉFGHi" = -1);
  assert (compare "abcdefgh" "ABCDeFGHi" = -1);
  assert (compare "abcdéfgh" "ACCDÉFGH" = -1);
  assert (compare "abcdéfgh" "ABCDÉFFH" = 1)

let _ =
  for i = 0 to 0xffff do
    if i < 0xd800 || i > 0xdfff then begin
      let s = to_utf_16 (conv i) in
(*Format.printf "%04x@." (String.length s);*)
      for j = 0 to String.length s / 2 - 2 do
        let c1 = get s (j * 2) + get s (j * 2 + 1) * 256 in
        let c2 = get s (j * 2 + 2) + get s (j * 2 + 3) * 256 in
        let v1 = combining_class c1 in
        let v2 = combining_class c2 in
(*Format.printf "%04x %04x => %02x %02x@." c1 c2 v1 v2;*)
(*        if v1 > 0 && v2 > 0 then Format.printf "%d %d@." v1 v2;*)
        assert (v1 = 0 || v2 = 0 || v1 <= v2)
      done
    end
  done

let _ =
let s = from_utf_16 "\x61\x00\x01\x03\x63\x00\x01\x03\x27\x03" in
order s;
assert (s = from_utf_16 "\x61\x00\x01\x03\x63\x00\x27\x03\x01\x03");
let s = from_utf_16 "\x01\x03\x27\x03" in
order s;
assert (s = from_utf_16 "\x27\x03\x01\x03")
(*
0061;LATIN SMALL LETTER A;...0;...
0063;LATIN SMALL LETTER C;...0;...
00E1;LATIN SMALL LETTER A WITH ACUTE;...0;...0061 0301;...
0107;LATIN SMALL LETTER C WITH ACUTE;...0;...0063 0301;...
0301;COMBINING ACUTE ACCENT;...230;...
0327;COMBINING CEDILLA;...202;...
*)
