/* Stub code for controlling terminals on Mac OS X. */

#ifndef WIN32
#include <sys/ioctl.h>
#endif
#include <caml/mlvalues.h>

CAMLprim value setControllingTerminal(value fdVal) {
#ifdef __APPLE__
  int fd = Int_val(fdVal); 
  return Val_int(ioctl(fd, TIOCSCTTY, (char *) 0));
#else
  return Val_int(0);
#endif
}

/* For debugging */
CAMLprim value dumpFd(value fdVal) {
  return fdVal;
}

#include <caml/fail.h>     // failwith
#ifndef WIN32
#include <sys/errno.h>     // errno
#endif
#include <string.h>        // strerror
#include <caml/alloc.h>    // alloc_tuple
#include <caml/memory.h>   // Store_field

#ifdef __CYGWIN__
#include <pty.h>           // openpty
#endif
#ifdef __linux__
#include <pty.h>           // openpty
#endif
#ifdef __APPLE__
#include <util.h>          // openpty
#endif
#ifdef __FreeBSD__
#include <sys/types.h>
#include <libutil.h>
#endif

/* c_openpty: unit -> (int * Unix.file_descr) */
CAMLprim value c_openpty() {
#if defined(__sun__) || defined(WIN32)
  failwith("openpty not implemented");
  return Val_int(0); // for type checker -- never reached.
#else  
  int master,slave;
  if (openpty(&master,&slave,NULL,NULL,NULL) < 0)
    failwith(strerror(errno));
  else {
    value pair = alloc_tuple(2);
    Store_field(pair,0,Val_int(master));
    Store_field(pair,1,Val_int(slave));
    return pair;
  }
#endif
}
