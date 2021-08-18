/* Unison file synchronizer: src/copy_stubs.c */
/* Copyright 2021-2023, Tõivo Leedjärv

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
*/

#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/version.h>

#if OCAML_VERSION_MAJOR < 5
#define caml_unix_error unix_error
#define caml_uerror uerror
#endif


#include <errno.h>


/* ----------------------------------------------- */
/* Clone a file given source and destination paths */
/* It must fully complete or fully fail.

   The function must not raise any exceptions.

   Return true for success and false for failure
   or if the operation is not supported.           */

#if defined(__APPLE__)


#include <AvailabilityMacros.h>

#if defined(MAC_OS_X_VERSION_10_12)
#include <string.h>
#include <sys/attr.h>
#include <sys/clonefile.h>

CAMLprim value unsn_clone_path(value src, value dst)
{
  CAMLparam2(src, dst);
  char *srcn, *dstn;
  int status;

  srcn = strdup(String_val(src));
  dstn = strdup(String_val(dst));
  caml_release_runtime_system();
  status = clonefile(srcn, dstn, CLONE_NOFOLLOW | CLONE_NOOWNERCOPY);
  free(srcn);
  free(dstn);
  caml_acquire_runtime_system();

  /* Don't raise an exception, just return false in case of errors */
  CAMLreturn(Val_bool(status == 0));
}
#else /* MAC_OS_X_VERSION_10_12 */
CAMLprim value unsn_clone_path(value src, value dst)
{
  CAMLparam2(src, dst);
  CAMLreturn(Val_false);
}
#endif /* MAC_OS_X_VERSION_10_12 */


#else /* defined(__APPLE__) */


CAMLprim value unsn_clone_path(value src, value dst)
{
  CAMLparam2(src, dst);
  CAMLreturn(Val_false);
}


#endif /* defined(__APPLE__) */


/* ----------------------------------------------- */
/* Clone a file given input and output fd          */
/* It must fully complete or fully fail.

   The function must not raise any exceptions.

   Return true for success and false for failure
   or if the operation is not supported.           */

#if defined(__linux__) || defined(__linux)


#include <sys/ioctl.h>

#if !defined(FICLONE) && defined(_IOW)
#define FICLONE _IOW(0x94, 9, int)
#endif

CAMLprim value unsn_clone_file(value in_fd, value out_fd)
{
  CAMLparam2(in_fd, out_fd);

#ifdef FICLONE
  caml_release_runtime_system();
  int status = ioctl(Int_val(out_fd), FICLONE, Int_val(in_fd));
  caml_acquire_runtime_system();

  /* Don't raise an exception, just return false in case of errors */
  CAMLreturn(Val_bool(status == 0));
#else /* defined(FICLONE) */
  CAMLreturn(Val_false);
#endif
}


#else /* defined(__linux__) */


CAMLprim value unsn_clone_file(value in_fd, value out_fd)
{
  CAMLparam2(in_fd, out_fd);
  CAMLreturn(Val_false);
}


#endif /* defined(__linux__) */


/* --------------------------------------------------------- */
/* Copy, or possibly clone, a file given input and output fd */
/* If operation is not supported by the OS or the filesystem
   then file offsets must not have been changed at failure.
   Output file offset must be changed on success.

   The function must return the number of bytes copied.

   On any error, raise a Unix exception based on errno.
   Raise ENOSYS if the operation is not supported.           */

#if defined(__linux__) || defined(__linux)


#include <unistd.h>
#include <sys/syscall.h>
#include <sys/sendfile.h>

CAMLprim value unsn_copy_file(value in_fd, value out_fd, value offs, value len)
{
  CAMLparam4(in_fd, out_fd, offs, len);
  off_t off_i = Int64_val(offs);
  ssize_t ret;

  caml_release_runtime_system();
#ifdef __NR_copy_file_range
  /* First, try copy_file_range() */
  /* Using off_i prevents changing in_fd file offset */
  ret = syscall(__NR_copy_file_range, Int_val(in_fd), &off_i, Int_val(out_fd), NULL, Long_val(len), 0);
  if (ret == -1 && (errno == ENOSYS || errno == EBADF || errno == EXDEV))
#endif /* defined(__NR_copy_file_range) */
  {
    /* Second, try sendfile(); this one changes out_fd file offset */
    ret = sendfile(Int_val(out_fd), Int_val(in_fd), &off_i, Long_val(len));
  }
  caml_acquire_runtime_system();
  if (ret == -1) caml_uerror("copy_file", Nothing);

  CAMLreturn(Val_long(ret));
}


#elif defined(__FreeBSD__)

#include <sys/param.h>
#include <sys/types.h>
#include <unistd.h>

CAMLprim value unsn_copy_file(value in_fd, value out_fd, value offs, value len)
{
  CAMLparam4(in_fd, out_fd, offs, len);
#if __FreeBSD_version >= 1300037
  off_t off_i = Int64_val(offs);
  ssize_t ret;

  caml_release_runtime_system();
  /* Using off_i prevents changing in_fd file offset */
  ret = copy_file_range(Int_val(in_fd), &off_i, Int_val(out_fd), NULL, Long_val(len), 0);
  caml_acquire_runtime_system();
  if (ret == -1) caml_uerror("copy_file", Nothing);

  CAMLreturn(Val_long(ret));
#else /* __FreeBSD_version >= 1300037 */
  caml_unix_error(ENOSYS, "copy_file", Nothing);
  CAMLreturn(Val_long(0));
#endif /* __FreeBSD_version >= 1300037 */
}


#elif defined(__sun) || defined(sun)


#include <sys/sendfile.h>

CAMLprim value unsn_copy_file(value in_fd, value out_fd, value offs, value len)
{
  CAMLparam4(in_fd, out_fd, offs, len);
  off_t off = orig_off = Int64_val(offs);
  ssize_t ret;

  caml_release_runtime_system();
  /* This one changes out_fd file offset */
  ret = sendfile(Int_val(out_fd), Int_val(in_fd), &off, Long_val(len));
  caml_acquire_runtime_system();
  if (ret == -1) {
    if (off > off_orig) {
      ret = off - off_orig;
    } else {
      caml_uerror("copy_file", Nothing);
    }
  }

  CAMLreturn(Val_long(ret));
}


#else /* defined(__linux__) || defined(__FreeBSD__) || defined(__sun) */


CAMLprim value unsn_copy_file(value in_fd, value out_fd, value offs, value len)
{
  CAMLparam4(in_fd, out_fd, offs, len);
  caml_unix_error(ENOSYS, "copy_file", Nothing);
  CAMLreturn(Val_long(0));
}


#endif /* defined(__linux__) || defined (__sun) */
