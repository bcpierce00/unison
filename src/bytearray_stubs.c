/* Unison file synchronizer: src/bytearray_stubs.c */
/* Copyright 1999-2020 (see COPYING for details) */

#include <string.h>

#include "caml/intext.h"
#include "caml/bigarray.h"
#include "caml/memory.h"


#define Array_data(a, i) (((char *) a->data) + Long_val(i))


CAMLprim value ml_blit_string_to_bigarray
(value s, value i, value a, value j, value l)
{
  char *src = String_val(s) + Long_val(i);
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
