/* $I1: Unison file synchronizer: src/osxsupport.c $ */
/* $I2: Last modified by vouillon on Thu, 25 Nov 2004 16:01:48 -0500 $ */
/* $I3: Copyright 1999-2004 (see COPYING for details) $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#ifdef __APPLE__
#include <sys/attr.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif
#include <errno.h>

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

CAMLprim value isMacOSX (value nothing) {
#ifdef __APPLE__
  return Val_true;
#else
  return Val_false;
#endif
}

/* FIX: this should be somewhere else */
/* Only used to check whether pty is supported */
CAMLprim value isLinux (value nothing) {
#ifdef __linux__
  return Val_true;
#else
  return Val_false;
#endif
}

CAMLprim value getFileInfos (value path, value need_size) {
#ifdef __APPLE__

  CAMLparam1(path);
  CAMLlocal3(res, fInfo, length);
  int retcode;
  struct attrlist attrList;
  unsigned long options = 0;
  struct {
    unsigned long length;
    char          finderInfo [32];
    off_t         rsrcLength;
  } attrBuf;

  attrList.bitmapcount = ATTR_BIT_MAP_COUNT;
  attrList.reserved = 0;
  attrList.commonattr = ATTR_CMN_FNDRINFO;
  attrList.volattr = 0;     /* volume attribute group */
  attrList.dirattr = 0;     /* directory attribute group */
  if (Bool_val (need_size))
    attrList.fileattr = ATTR_FILE_RSRCLENGTH;    /* file attribute group */
  else
    attrList.fileattr = 0;
  attrList.forkattr = 0;    /* fork attribute group */

  retcode = getattrlist(String_val (path), &attrList, &attrBuf,
                        sizeof attrBuf, options);

  if (retcode == -1) uerror("getattrlist", path);

  fInfo = alloc_string (32);
  memcpy (String_val (fInfo), attrBuf.finderInfo, 32);
  if (Bool_val (need_size))
    length = copy_int64 (attrBuf.rsrcLength);
  else
    length = copy_int64 (0);

  res = alloc_small (2, 0);
  Field (res, 0) = fInfo;
  Field (res, 1) = length;

  CAMLreturn (res);

#else

  unix_error (ENOSYS, "getattrlist", path);

#endif
}

CAMLprim value setFileInfos (value path, value fInfo) {
#ifdef __APPLE__

  CAMLparam2(path, fInfo);
  int retcode;
  struct attrlist attrList;
  unsigned long options = 0;
  struct {
    unsigned long length;
    char          finderInfo [32];
  } attrBuf;

  attrList.bitmapcount = ATTR_BIT_MAP_COUNT;
  attrList.reserved = 0;
  attrList.commonattr = ATTR_CMN_FNDRINFO;
  attrList.volattr = 0;     /* volume attribute group */
  attrList.dirattr = 0;     /* directory attribute group */
  attrList.fileattr = 0;    /* file attribute group */
  attrList.forkattr = 0;    /* fork attribute group */

  memcpy (attrBuf.finderInfo, String_val (fInfo), 32);

  retcode = setattrlist(String_val (path), &attrList, attrBuf.finderInfo,
                        sizeof attrBuf.finderInfo, options);

  if (retcode == -1) uerror("setattrlist", path);

  CAMLreturn (Val_unit);

#else

  unix_error (ENOSYS, "setattrlist", path);

#endif
}
