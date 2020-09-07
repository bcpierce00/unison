(* Unison file synchronizer: src/ubase/umarshal.ml *)
(* Copyright 2020, Stéphane Glondu

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

exception Error of string

type 'a t = {
    read : (int -> bytes * int) -> 'a;
    write : (bytes -> int -> int -> unit) -> 'a -> unit;
}

external id : 'a -> 'a = "%identity"

let header_size = 8

let max_int_int64 = Int64.of_int max_int
let min_int_int64 = Int64.of_int min_int

let data_size header offset =
  if offset + header_size <= Bytes.length header then
    let n = Bytes.get_int64_be header offset in
    if n < 0L then
      raise (Error "data_size: negative size")
    else if n <= max_int_int64 then
      Int64.to_int n
    else
      raise (Error "data_size: payload too large")
  else
    raise (Error "data_size: header too short")

let to_string m x =
  let buffer = Buffer.create 1024 in
  m.write (Buffer.add_subbytes buffer) x;
  let header = Bytes.create header_size in
  Bytes.set_int64_be header 0 (Int64.of_int (Buffer.length buffer));
  Bytes.to_string header ^ Buffer.contents buffer

let from_bytes m buffer offset =
  let length = Bytes.length buffer in
  let pos = ref (offset + header_size) in
  m.read (fun n ->
      let i = !pos in
      if i + n <= length then (
        pos := i + n;
        buffer, i
      ) else (
        raise (Error "from_bytes: end of input")
      )
    )

let unit =
  {
    read = (fun _ -> ());
    write = (fun _ () -> ());
  }

let char =
  {
    read =
      (fun recv ->
        let buffer, offset = recv 1 in
        Bytes.unsafe_get buffer offset
      );
    write =
      (fun send x ->
        let res = Bytes.create 1 in
        Bytes.unsafe_set res 0 x;
        send res 0 1
      );
  }

let int64 =
  {
    read =
      (fun recv ->
        let realize n get of_int =
          let buffer, offset = recv n in
          of_int (get buffer offset)
        in
        match int_of_char (char.read recv) with
        | 0 -> 0L
        | 1 -> realize 1 Bytes.get_int8 Int64.of_int
        | 2 -> realize 2 Bytes.get_int16_be Int64.of_int
        | 4 -> realize 4 Bytes.get_int32_be Int64.of_int32
        | 8 -> realize 8 Bytes.get_int64_be id
        | n -> raise (Error (Printf.sprintf "int64.read: unexpected size (%d)" n))
      );
    write =
      (fun send x ->
        let realize n set to_int =
          let buffer = Bytes.create (1 + n) in
          Bytes.unsafe_set buffer 0 (char_of_int n);
          set buffer 1 (to_int x);
          send buffer 0 (1 + n)
        in
        if x = 0L then
          char.write send '\000'
        else if -0x80L <= x && x < 0x80L then
          realize 1 Bytes.set_int8 Int64.to_int
        else if -0x8000L <= x && x < 0x8000L then
          realize 2 Bytes.set_int16_be Int64.to_int
        else if -0x8000_0000L <= x && x < 0x8000_0000L then
          realize 4 Bytes.set_int32_be Int64.to_int32
        else
          realize 8 Bytes.set_int64_be id
      );
  }

let int =
  {
    read =
      (fun recv ->
        let r = int64.read recv in
        if r < min_int_int64 || r > max_int_int64 then
          raise (Error "int.read: too large")
        else
          Int64.to_int r
      );
    write =
      (fun send x ->
        int64.write send (Int64.of_int x)
      );
  }

let string =
  {
    read =
      (fun recv ->
        let length = int.read recv in
        let buffer, offset = recv length in
        Bytes.sub_string buffer offset length
      );
    write =
      (fun send x ->
        let length = String.length x in
        int.write send length;
        send (Bytes.of_string x) 0 length
      );
  }

let float =
  {
    read =
      (fun recv ->
        Int64.float_of_bits (int64.read recv)
      );
    write =
      (fun send x ->
        int64.write send (Int64.bits_of_float x)
      );
  }

let list m =
  {
    read =
      (fun recv ->
        let length = int.read recv in
        let result = ref [] in
        for _ = 1 to length do
          result := m.read recv :: !result
        done;
        List.rev !result
      );
    write =
      (fun send x ->
        int.write send (List.length x);
        List.iter (fun x -> m.write send x) x
      );
  }

let prod2 ma mb f g =
  {
    read =
      (fun recv ->
        let a = ma.read recv in
        let b = mb.read recv in
        g (a, b)
      );
    write =
      (fun send x ->
        let a, b = f x in
        ma.write send a;
        mb.write send b
      );
  }

let prod3 ma mb mc f g =
  {
    read =
      (fun recv ->
        let a = ma.read recv in
        let b = mb.read recv in
        let c = mc.read recv in
        g (a, b, c)
      );
    write =
      (fun send x ->
        let a, b, c = f x in
        ma.write send a;
        mb.write send b;
        mc.write send c
      );
  }

let prod4 ma mb mc md f g =
  {
    read =
      (fun recv ->
        let a = ma.read recv in
        let b = mb.read recv in
        let c = mc.read recv in
        let d = md.read recv in
        g (a, b, c, d)
      );
    write =
      (fun send x ->
        let a, b, c, d = f x in
        ma.write send a;
        mb.write send b;
        mc.write send c;
        md.write send d
      );
  }

let prod6 ma mb mc md me mf f g =
  {
    read =
      (fun recv ->
        let a = ma.read recv in
        let b = mb.read recv in
        let c = mc.read recv in
        let d = md.read recv in
        let e = me.read recv in
        let f = mf.read recv in
        g (a, b, c, d, e, f)
      );
    write =
      (fun send x ->
        let a, b, c, d, e, f = f x in
        ma.write send a;
        mb.write send b;
        mc.write send c;
        md.write send d;
        me.write send e;
        mf.write send f
      );
  }

type ('a, 'b) sum2 = I21 of 'a | I22 of 'b

let sum2 ma mb f g =
  {
    read =
      (fun recv ->
        g (match char.read recv with
           | '\000' -> I21 (ma.read recv)
           | '\001' -> I22 (mb.read recv)
           | _ -> raise (Error "sum2: invalid tag"))
      );
    write =
      (fun send x ->
        match f x with
        | I21 a -> char.write send '\000'; ma.write send a
        | I22 a -> char.write send '\001'; mb.write send a
      );
  }

let option m =
  sum2 unit m
    (function
     | None -> I21 ()
     | Some a -> I22 a)
    (function
     | I21 () -> None
     | I22 a -> Some a)

type ('a, 'b, 'c) sum3 = I31 of 'a | I32 of 'b | I33 of 'c

let sum3 ma mb mc f g =
  {
    read =
      (fun recv ->
        g (match char.read recv with
           | '\000' -> I31 (ma.read recv)
           | '\001' -> I32 (mb.read recv)
           | '\002' -> I33 (mc.read recv)
           | _ -> raise (Error "sum3: invalid tag"))
      );
    write =
      (fun send x ->
        match f x with
        | I31 a -> char.write send '\000'; ma.write send a
        | I32 a -> char.write send '\001'; mb.write send a
        | I33 a -> char.write send '\002'; mc.write send a
      );
  }
