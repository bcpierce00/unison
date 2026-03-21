/* Unison file synchronizer: src/bytearray_stubs.c */
/* Copyright 1999-2020 (see COPYING for details) */

#include <string.h>

#include <caml/bigarray.h>
#include <caml/memory.h>

#define Array_data(a, i) (((char *) Caml_ba_data_val(a)) + Long_val(i))

CAMLprim value ml_blit_bytes_to_bigarray
(value s, value i, value a, value j, value l)
{
  CAMLparam5(s, i, a, j, l);
  unsigned char *src = Bytes_val(s) + Long_val(i);
  char *dest = Array_data(a, j);
  memcpy(dest, src, Long_val(l));
  CAMLreturn(Val_unit);
}

CAMLprim value ml_blit_string_to_bigarray
(value s, value i, value a, value j, value l)
{
  return ml_blit_bytes_to_bigarray(s, i, a, j, l);
}

CAMLprim value ml_blit_bigarray_to_bytes
(value a, value i, value s, value j, value l)
{
  CAMLparam5(a, i, s, j, l);
  char *src = Array_data(a, i);
  unsigned char *dest = Bytes_val(s) + Long_val(j);
  memcpy(dest, src, Long_val(l));
  CAMLreturn(Val_unit);
}
