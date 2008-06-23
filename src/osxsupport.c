/* Unison file synchronizer: src/osxsupport.c */
/* Copyright 1999-2008 (see COPYING for details) */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#ifdef __APPLE__
#include <sys/types.h>
#include <sys/stat.h>
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

  if (Bool_val (need_size)) {
    if (attrBuf.length != sizeof attrBuf)
      unix_error (EOPNOTSUPP, "getattrlist", path);
  } else {
    if (attrBuf.length < sizeof (unsigned long) + 32)
      unix_error (EOPNOTSUPP, "getattrlist", path);
  }

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

  if (retcode == -1 && errno == EACCES) {
    /* Unlike with normal Unix attributes, we cannot set OS X attributes 
       if file is read-only.  Try making it writable temporarily. */
    struct stat st;
    int r = stat(String_val(path), &st);
    if (r == -1) uerror("setattrlist", path);
    r = chmod(String_val(path), st.st_mode | S_IWUSR);
    if (r == -1) uerror("setattrlist", path);
    /* Try again */
    retcode = setattrlist(String_val (path), &attrList, attrBuf.finderInfo,
                          sizeof attrBuf.finderInfo, options);
    /* Whether or not that worked, we should try to set the mode back. */
    chmod(String_val(path), st.st_mode);
  }

  if (retcode == -1) uerror("setattrlist", path);

  CAMLreturn (Val_unit);

#else

  unix_error (ENOSYS, "setattrlist", path);

#endif
}
