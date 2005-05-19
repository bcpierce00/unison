/* Stub code for controlling terminals on Mac OS X. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>    // alloc_tuple
#include <caml/memory.h>   // Store_field
#include <caml/fail.h>     // failwith

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

// openpty
#if defined(__linux)
#include <pty.h>
#define HAS_OPENPTY 1
#endif

#ifdef __APPLE__
#include <util.h>
#define HAS_OPENPTY 1
#endif

#ifdef __FreeBSD__
#include <libutil.h>
#define HAS_OPENPTY 1
#endif

#ifdef HAS_OPENPTY

#include <sys/ioctl.h>
#include <sys/types.h>

CAMLprim value setControllingTerminal(value fdVal) {
  int fd = Int_val(fdVal);
  if (ioctl(fd, TIOCSCTTY, (char *) 0) < 0)
    uerror("ioctl", NULL);
  return Val_unit;
}

/* c_openpty: unit -> (int * Unix.file_descr) */
CAMLprim value c_openpty() {
  int master,slave;
  if (openpty(&master,&slave,NULL,NULL,NULL) < 0)
    uerror("openpty", NULL);
  value pair = alloc_tuple(2);
  Store_field(pair,0,Val_int(master));
  Store_field(pair,1,Val_int(slave));
  return pair;
}

#else // not HAS_OPENPTY

CAMLprim value setControllingTerminal(value fdVal) {
  unix_error (ENOSYS, "setControllingTerminal", NULL);
}

CAMLprim value c_openpty() {
  unix_error (ENOSYS, "openpty", NULL);
}

#endif
