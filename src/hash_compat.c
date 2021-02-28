/* The pre-OCaml 4.00 hash implementation */
/* FIXME: This is included for backwards compatibility only and must be
 * REMVOED at next Unison version increase. The removal of this will
 * break Unison version compatibility. */

/* Code copied from OCaml sources */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/address_class.h"

struct hash_state {
  uintnat accu;
  intnat univ_limit, univ_count;
};

static void hash_aux(struct hash_state*, value obj);

CAMLprim value unsn_hash_univ_param(value count, value limit, value obj)
{
  struct hash_state h;
  h.univ_limit = Long_val(limit);
  h.univ_count = Long_val(count);
  h.accu = 0;
  hash_aux(&h, obj);
  return Val_long(h.accu & 0x3FFFFFFF);
  /* The & has two purposes: ensure that the return value is positive
     and give the same result on 32 bit and 64 bit architectures. */
}

#define Alpha 65599
#define Beta 19
#define Combine(new)  (h->accu = h->accu * Alpha + (new))
#define Combine_small(new) (h->accu = h->accu * Beta + (new))

static void hash_aux(struct hash_state* h, value obj)
{
  unsigned char * p;
  mlsize_t i, j;
  tag_t tag;

  h->univ_limit--;
  if (h->univ_count < 0 || h->univ_limit < 0) return;

 again:
  if (Is_long(obj)) {
    h->univ_count--;
    Combine(Long_val(obj));
    return;
  }
  if (! Is_in_value_area(obj)) {
    /* obj is a pointer outside the heap, to an object with
       a priori unknown structure. Use its physical address as hash key. */
    Combine((intnat) obj);
    return;
  }
  /* Pointers into the heap are well-structured blocks. So are atoms.
     We can inspect the block contents. */
  /* The code needs reindenting later. Leaving as is to facilitate review. */
    tag = Tag_val(obj);
    switch (tag) {
    case String_tag:
      h->univ_count--;
      i = caml_string_length(obj);
      for (p = &Byte_u(obj, 0); i > 0; i--, p++)
        Combine_small(*p);
      break;
    case Double_tag:
      /* For doubles, we inspect their binary representation, LSB first.
         The results are consistent among all platforms with IEEE floats. */
      h->univ_count--;
#ifdef ARCH_BIG_ENDIAN
      for (p = &Byte_u(obj, sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, 0), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      break;
    case Double_array_tag:
      h->univ_count--;
      for (j = 0; j < Bosize_val(obj); j += sizeof(double)) {
#ifdef ARCH_BIG_ENDIAN
      for (p = &Byte_u(obj, j + sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, j), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      }
      break;
    case Abstract_tag:
      /* We don't know anything about the contents of the block.
         Better do nothing. */
      break;
    case Infix_tag:
      hash_aux(h, obj - Infix_offset_val(obj));
      break;
    case Forward_tag:
      obj = Forward_val (obj);
      goto again;
    case Object_tag:
      h->univ_count--;
      Combine(Oid_val(obj));
      break;
    case Custom_tag:
      /* If no hashing function provided, do nothing */
      if (Custom_ops_val(obj)->hash != NULL) {
        h->univ_count--;
        Combine(Custom_ops_val(obj)->hash(obj));
      }
      break;
#ifdef NO_NAKED_POINTERS
    case Closure_tag:
      h->univ_count--;
      Combine_small(tag);
      /* Recursively hash the environment fields */
      i = Wosize_val(obj);
      j = Start_env_closinfo(Closinfo_val(obj));
      while (i > j) {
        i--;
        hash_aux(h, Field(obj, i));
      }
      /* Combine the code pointers, closure info fields, and infix headers */
      while (i > 0) {
        i--;
        Combine(Field(obj, i));
        h->univ_count--;
      }
      break;
#endif
    default:
      h->univ_count--;
      Combine_small(tag);
      i = Wosize_val(obj);
      while (i != 0) {
        i--;
        hash_aux(h, Field(obj, i));
      }
      break;
    }
}

