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
    read : (bytes -> int -> int -> unit) -> 'a;
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
  let offset = ref (offset + header_size) in
  m.read (fun buffer' offset' n ->
      let i = !offset in
      if i + n <= length then (
        offset := i + n;
        Bytes.blit buffer i buffer' offset' n
      ) else (
        raise (Error "from_bytes: end of input")
      )
    )

let from_string m buffer offset =
  from_bytes m (Bytes.of_string buffer) offset

let from_channel m ic =
  let header = Bytes.create header_size in
  really_input ic header 0 header_size;
  m.read (really_input ic)

let to_channel m oc x =
  let header = Bytes.create header_size in
  let header_pos = pos_out oc in
  output oc header 0 header_size;
  m.write (output oc) x;
  let end_pos = pos_out oc in
  let data_size = end_pos - header_pos - header_size in
  Bytes.set_int64_be header 0 (Int64.of_int data_size);
  seek_out oc header_pos;
  output oc header 0 header_size;
  seek_out oc end_pos

let rec1 a =
  let rec fa =
    {
      read = (fun recv -> (a fa).read recv);
      write = (fun send x -> (a fa).write send x);
    }
  in
  fa

let rec2 a b =
  let rec fa =
    {
      read = (fun recv -> (a fb).read recv);
      write = (fun send x -> (a fb).write send x);
    }
  and fb =
    {
      read = (fun recv -> (b fa).read recv);
      write = (fun send x -> (b fa).write send x);
    }
  in
  (fb, fa)

let unit =
  {
    read = (fun _ -> ());
    write = (fun _ () -> ());
  }

let char =
  {
    read =
      (fun recv ->
        let buffer = Bytes.create 1 in
        recv buffer 0 1;
        Bytes.unsafe_get buffer 0
      );
    write =
      (fun send x ->
        let res = Bytes.create 1 in
        Bytes.unsafe_set res 0 x;
        send res 0 1
      );
  }

let bool =
  {
    read =
      (fun recv ->
        match char.read recv with
        | '\000' -> false
        | '\001' -> true
        | _ -> raise (Error "bool: invalid value")
      );
    write =
      (fun send x ->
        char.write send (if x then '\001' else '\000')
      );
  }

let int32 =
  {
    read =
      (fun recv ->
        let buffer = Bytes.create 4 in
        recv buffer 0 4;
        Bytes.get_int32_be buffer 0
      );
    write =
      (fun send x ->
        let res = Bytes.create 4 in
        Bytes.set_int32_be res 0 x;
        send res 0 4
      );
  }

let int64 =
  {
    read =
      (fun recv ->
        let realize n get of_int =
          let buffer = Bytes.create n in
          recv buffer 0 n;
          of_int (get buffer 0)
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
        let buffer = Bytes.create length in
        recv buffer 0 length;
        Bytes.to_string buffer
      );
    write =
      (fun send x ->
        let length = String.length x in
        int.write send length;
        send (Bytes.of_string x) 0 length
      );
  }

type bytearray =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external unsafe_blit_from_bytes : bytes -> int -> bytearray -> int -> int -> unit
  = "ml_blit_string_to_bigarray" [@@noalloc]

external unsafe_blit_to_bytes : bytearray -> int -> bytes -> int -> int -> unit
  = "ml_blit_bigarray_to_string" [@@noalloc]

let bytearray =
  {
    read =
      (fun recv ->
        let length = int.read recv in
        let res = Bigarray.(Array1.create char c_layout length) in
        let rec loop offset length =
          if length > 0 then (
            let sub_length = min length Sys.max_string_length in
            let buffer = Bytes.create sub_length in
            recv buffer 0 sub_length;
            unsafe_blit_from_bytes buffer 0 res offset sub_length;
            loop (offset + sub_length) (length - sub_length)
          )
        in
        loop 0 length;
        res
      );
    write =
      (fun send x ->
        let length = Bigarray.Array1.dim x in
        int.write send length;
        let buffer = Bytes.create (min length Sys.max_string_length) in
        let rec loop offset length =
          if length > 0 then (
            let sub_length = min length Sys.max_string_length in
            unsafe_blit_to_bytes x offset buffer 0 sub_length;
            send buffer 0 sub_length;
            loop (offset + sub_length) (length - sub_length)
          )
        in
        loop 0 length
      );
  }

let marshal_to_bytearray m x =
  let data_size = ref 0 in
  m.write (fun _ _ length -> data_size := !data_size + length) x;
  let header = Bytes.create header_size in
  Bytes.set_int64_be header 0 (Int64.of_int !data_size);
  let total_size = header_size + !data_size in
  let result = Bigarray.(Array1.create char c_layout total_size) in
  unsafe_blit_from_bytes header 0 result 0 header_size;
  let offset = ref header_size in
  m.write (fun buffer offset' length ->
      let i = !offset in
      if i + length <= total_size then (
        unsafe_blit_from_bytes buffer offset' result i length;
        offset := i + length
      ) else (
        raise (Error "marshal_to_bytearray: length inconsistency")
      )
    ) x;
  if !offset <> total_size then
    raise (Error "marshal_to_bytearray: universe inconsistency");
  result

let unmarshal_from_bytearray m x offset =
  let length = Bigarray.Array1.dim x in
  let offset = ref (offset + header_size) in
  m.read (fun buffer' offset' n ->
      let i = !offset in
      if i + n <= length then (
        offset := i + n;
        unsafe_blit_to_bytes x i buffer' offset' n
      ) else (
        raise (Error "unmarshal_from_bytearray: end of input")
      )
    )

type int32bigarray =
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

let int32bigarray =
  {
    read =
      (fun recv ->
        let length = int.read recv in
        let res = Bigarray.(Array1.create int32 c_layout length) in
        for i = 0 to length - 1 do
          res.{i} <- int32.read recv
        done;
        res
      );
    write =
      (fun send x ->
        let length = Bigarray.Array1.dim x in
        int.write send length;
        for i = 0 to length - 1 do
          int32.write send x.{i}
        done
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

let prod5 ma mb mc md me f g =
  {
    read =
      (fun recv ->
        let a = ma.read recv in
        let b = mb.read recv in
        let c = mc.read recv in
        let d = md.read recv in
        let e = me.read recv in
        g (a, b, c, d, e)
      );
    write =
      (fun send x ->
        let a, b, c, d, e = f x in
        ma.write send a;
        mb.write send b;
        mc.write send c;
        md.write send d;
        me.write send e
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

let sum1 ma f g =
  {
    read = (fun recv -> g (ma.read recv));
    write = (fun send x -> ma.write send (f x));
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

type ('a, 'b, 'c, 'd) sum4 = I41 of 'a | I42 of 'b | I43 of 'c | I44 of 'd

let sum4 ma mb mc md f g =
  {
    read =
      (fun recv ->
        g (match char.read recv with
           | '\000' -> I41 (ma.read recv)
           | '\001' -> I42 (mb.read recv)
           | '\002' -> I43 (mc.read recv)
           | '\003' -> I44 (md.read recv)
           | _ -> raise (Error "sum4: invalid tag"))
      );
    write =
      (fun send x ->
        match f x with
        | I41 a -> char.write send '\000'; ma.write send a
        | I42 a -> char.write send '\001'; mb.write send a
        | I43 a -> char.write send '\002'; mc.write send a
        | I44 a -> char.write send '\003'; md.write send a
      );
  }

type ('a, 'b, 'c, 'd, 'e) sum5 = I51 of 'a | I52 of 'b | I53 of 'c | I54 of 'd | I55 of 'e

let sum5 ma mb mc md me f g =
  {
    read =
      (fun recv ->
        g (match char.read recv with
           | '\000' -> I51 (ma.read recv)
           | '\001' -> I52 (mb.read recv)
           | '\002' -> I53 (mc.read recv)
           | '\003' -> I54 (md.read recv)
           | '\004' -> I55 (me.read recv)
           | _ -> raise (Error "sum5: invalid tag"))
      );
    write =
      (fun send x ->
        match f x with
        | I51 a -> char.write send '\000'; ma.write send a
        | I52 a -> char.write send '\001'; mb.write send a
        | I53 a -> char.write send '\002'; mc.write send a
        | I54 a -> char.write send '\003'; md.write send a
        | I55 a -> char.write send '\004'; me.write send a
      );
  }

type ('a, 'b, 'c, 'd, 'e, 'f) sum6 = I61 of 'a | I62 of 'b | I63 of 'c | I64 of 'd | I65 of 'e | I66 of 'f

let sum6 ma mb mc md me mf f g =
  {
    read =
      (fun recv ->
        g (match char.read recv with
           | '\000' -> I61 (ma.read recv)
           | '\001' -> I62 (mb.read recv)
           | '\002' -> I63 (mc.read recv)
           | '\003' -> I64 (md.read recv)
           | '\004' -> I65 (me.read recv)
           | '\005' -> I66 (mf.read recv)
           | _ -> raise (Error "sum6: invalid tag"))
      );
    write =
      (fun send x ->
        match f x with
        | I61 a -> char.write send '\000'; ma.write send a
        | I62 a -> char.write send '\001'; mb.write send a
        | I63 a -> char.write send '\002'; mc.write send a
        | I64 a -> char.write send '\003'; md.write send a
        | I65 a -> char.write send '\004'; me.write send a
        | I66 a -> char.write send '\005'; mf.write send a
      );
  }

module type PROPLIST_S = sig
  type key = string
  type value = Obj.t
  type map
  val cardinal : map -> int
  val empty : map
  val add : key -> value -> map -> map
  val iter : (key -> value -> unit) -> map -> unit
  val find_m : key -> value t
end

module Proplist (S : PROPLIST_S) = struct
  let m =
    {
      read =
        (fun recv ->
          let length = int.read recv in
          let res = ref S.empty in
          for _ = 1 to length do
            let key = string.read recv in
            let value = (S.find_m key).read recv in
            res := S.add key value !res
          done;
          !res
        );
      write =
        (fun send x ->
          let length = S.cardinal x in
          int.write send length;
          S.iter (fun key value ->
              string.write send key;
              (S.find_m key).write send value
            ) x
        );
    }
end
