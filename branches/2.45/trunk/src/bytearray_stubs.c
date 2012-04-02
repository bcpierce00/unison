/* Unison file synchronizer: src/bytearray_stubs.c */
/* Copyright 1999-2012 (see COPYING for details) */

#include <string.h>

#include "caml/intext.h"
#include "caml/bigarray.h"

CAMLprim value ml_marshal_to_bigarray(value v, value flags)
{
  char *buf;
  long len;
  output_value_to_malloc(v, flags, &buf, &len);
  return alloc_bigarray(BIGARRAY_UINT8 | BIGARRAY_C_LAYOUT | BIGARRAY_MANAGED,
                        1, buf, &len);
}


#define Array_data(a, i) (((char *) a->data) + Long_val(i))


CAMLprim value ml_unmarshal_from_bigarray(value b, value ofs)
{
  struct caml_bigarray *b_arr = Bigarray_val(b);
  return input_value_from_block (Array_data (b_arr, ofs),
                                 b_arr->dim[0] - Long_val(ofs));
}

CAMLprim value ml_blit_string_to_bigarray
(value s, value i, value a, value j, value l)
{
  char *src = String_val(s) + Int_val(i);
  char *dest = Array_data(Bigarray_val(a), j);
  memcpy(dest, src, Long_val(l));
  return Val_unit;
}

CAMLprim value ml_blit_bigarray_to_string
(value a, value i, value s, value j, value l)
{
  char *src = Array_data(Bigarray_val(a), i);
  char *dest = String_val(s) + Long_val(j);
  memcpy(dest, src, Long_val(l));
  return Val_unit;
}
