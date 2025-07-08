/* Unison file synchronizer: src/copy_stubs.c */
/* Copyright 2021-2025, Tõivo Leedjärv

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
#define caml_win32_maperr win32_maperr
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

CAMLprim value unison_clone_path(value src, value dst)
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
CAMLprim value unison_clone_path(value src, value dst)
{
  CAMLparam2(src, dst);
  CAMLreturn(Val_false);
}
#endif /* MAC_OS_X_VERSION_10_12 */


#else /* defined(__APPLE__) */


CAMLprim value unison_clone_path(value src, value dst)
{
  CAMLparam2(src, dst);
  CAMLreturn(Val_false);
}

/* Regarding Windows API: The CopyFile function in Windows API can do file
 * clones under right conditions. Nevertheless, we can't use that function
 * since it is not possible to explicitly request cloning (it is possible to
 * check whether block cloning is supported (see [unison_copy_file]) but it is
 * not a guarantee that the CopyFile function will actually result in a
 * cloning operation) and the function will fall back to a normal copy, which
 * we do not want in this case (even though performance-wise it would be more
 * like sendfile). */

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

CAMLprim value unison_clone_file(value in_fd, value out_fd)
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


CAMLprim value unison_clone_file(value in_fd, value out_fd)
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

CAMLprim value unison_copy_file(value in_fd, value out_fd, value in_offs, value len)
{
  CAMLparam4(in_fd, out_fd, in_offs, len);
  off_t off_i = Int64_val(in_offs);
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

CAMLprim value unison_copy_file(value in_fd, value out_fd, value in_offs, value len)
{
  CAMLparam4(in_fd, out_fd, in_offs, len);
#if __FreeBSD_version >= 1300037
  off_t off_i = Int64_val(in_offs);
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

CAMLprim value unison_copy_file(value in_fd, value out_fd, value in_offs, value len)
{
  CAMLparam4(in_fd, out_fd, in_offs, len);
  off_t off_orig;
  off_t off = off_orig = Int64_val(in_offs);
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


#elif defined(_WIN32)


#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif

#include <windows.h>
#include <winioctl.h>

#if defined(__MINGW32__)

/* FIXME: As of January 2025, mingw has does not include type definitions for
 * FSCTL_GET_INTEGRITY_INFORMATION_BUFFER and DUPLICATE_EXTENTS_DATA (but does
 * include the definitions of FSCTL constants). If you get a compilation error
 * about "type redefinition" or similar, just remove the typedefs below. */

#ifndef FILE_SUPPORTS_BLOCK_REFCOUNTING
#define FILE_SUPPORTS_BLOCK_REFCOUNTING 0x08000000
#endif

#ifndef FSCTL_GET_INTEGRITY_INFORMATION
#define FSCTL_GET_INTEGRITY_INFORMATION 0x9027c
#endif

typedef struct _FSCTL_GET_INTEGRITY_INFORMATION_BUFFER {
  WORD  ChecksumAlgorithm;
  WORD  Reserved;
  DWORD Flags;
  DWORD ChecksumChunkSizeInBytes;
  DWORD ClusterSizeInBytes;
} FSCTL_GET_INTEGRITY_INFORMATION_BUFFER;

#ifndef FSCTL_DUPLICATE_EXTENTS_TO_FILE
#define FSCTL_DUPLICATE_EXTENTS_TO_FILE 0x98344
#endif

#if !defined(__MINGW64_VERSION_MAJOR) || __MINGW64_VERSION_MAJOR < 13

/* This typedef is included in mingw since version 13.0.0 */
typedef struct _DUPLICATE_EXTENTS_DATA {
  HANDLE        FileHandle;
  LARGE_INTEGER SourceFileOffset;
  LARGE_INTEGER TargetFileOffset;
  LARGE_INTEGER ByteCount;
} DUPLICATE_EXTENTS_DATA;

#endif /* __MINGW64_VERSION_MAJOR < 13 */

#endif /* defined(__MINGW32__) */

void unsn_copy_file_error(void)
{
  caml_win32_maperr(GetLastError());
  caml_uerror("copy_file", Nothing);
}

BOOL unsn_copy_file_supports_cloning(HANDLE h)
{
  DWORD flags = 0;
  return
    (GetVolumeInformationByHandleW(h, NULL, 0, NULL, NULL, &flags, NULL, 0)
      && (flags & FILE_SUPPORTS_BLOCK_REFCOUNTING));
 }

void unsn_copy_file_set_sparse(HANDLE h, BOOL sparse)
{
  FILE_SET_SPARSE_BUFFER sp;
  sp.SetSparse = sparse;
  if (!DeviceIoControl(h, FSCTL_SET_SPARSE, &sp, sizeof(sp),
        NULL, 0, NULL, NULL)) {
    unsn_copy_file_error();
  }
}

BOOL unsn_copy_file_get_sparse(HANDLE h)
{
  FILE_BASIC_INFO info;
  if (!GetFileInformationByHandleEx(h, FileBasicInfo, &info, sizeof(info))) {
    unsn_copy_file_error();
  }
  return (info.FileAttributes & FILE_ATTRIBUTE_SPARSE_FILE) != 0;
}

CAMLprim value unison_copy_file(value in_fd, value out_fd, value in_offs, value len)
{
  CAMLparam4(in_fd, out_fd, in_offs, len);

  HANDLE hin = Handle_val(in_fd);
  HANDLE hout = Handle_val(out_fd);

  if (!unsn_copy_file_supports_cloning(hout)
      || !unsn_copy_file_supports_cloning(hin)) {
    caml_unix_error(ENOSYS, "copy_file", Nothing);
  }

  /* INTEGRITY_INFORMATION contains information about fs cluster size. We make
   * the assumption that both the source and destination have the same cluster
   * size (From MSDN: The source and destination files must be on the same ReFS
   * volume.). */
  FSCTL_GET_INTEGRITY_INFORMATION_BUFFER intgrty;
  if (!DeviceIoControl(hin, FSCTL_GET_INTEGRITY_INFORMATION,
        NULL, 0, &intgrty, sizeof(intgrty), NULL, NULL)) {
    unsn_copy_file_error();
  }

  DUPLICATE_EXTENTS_DATA dupl;
  dupl.FileHandle = hin;
  dupl.SourceFileOffset.QuadPart = Int64_val(in_offs);
  if (!SetFilePointerEx(hout, (LARGE_INTEGER){0}, &dupl.TargetFileOffset,
        FILE_CURRENT)) {
    unsn_copy_file_error();
  }

  /* From MSDN: The source and destination regions must begin and end at
   * a cluster boundary. */
  if ((dupl.SourceFileOffset.QuadPart % intgrty.ClusterSizeInBytes != 0)
      || (dupl.TargetFileOffset.QuadPart % intgrty.ClusterSizeInBytes != 0)) {
    /* FSCTL_DUPLICATE_EXTENTS_TO_FILE would fail anyway if this didn't hold,
     * but this way we can bail out earlier. */
    caml_unix_error(EINVAL, "copy_file", Nothing);
  }
  dupl.ByteCount.QuadPart = Long_val(len) +
    intgrty.ClusterSizeInBytes - (Long_val(len) % intgrty.ClusterSizeInBytes);

  /* From MSDN: If the source file is sparse, the destination file must also be
   * sparse. */
  BOOL in_is_sparse = unsn_copy_file_get_sparse(hin);
  BOOL out_is_sparse = unsn_copy_file_get_sparse(hout);
  /* Based on the wording in MSDN, it is ok if the destination file is sparse
   * and the source is not, the opposite is not. */
  if (in_is_sparse && !out_is_sparse) {
    unsn_copy_file_set_sparse(hout, TRUE);
  }

  /* From MSDN: The destination region must not extend past the end of file. */
  LARGE_INTEGER newTargetLen;
  newTargetLen.QuadPart = dupl.TargetFileOffset.QuadPart + Long_val(len);
  FILE_STANDARD_INFO fi;
  if (!GetFileInformationByHandleEx(hout, FileStandardInfo, &fi, sizeof(fi))) {
    unsn_copy_file_error();
  }
  if (fi.EndOfFile.QuadPart < newTargetLen.QuadPart) {
    /* We want to avoid allocating on disk when extending the file. Make the
     * destination file temporarily sparse, if it's not already. */
    if (!out_is_sparse && !in_is_sparse) {
      unsn_copy_file_set_sparse(hout, TRUE);
    }
    FILE_END_OF_FILE_INFO eofi;
    eofi.EndOfFile = newTargetLen;
    if (!SetFileInformationByHandle(hout, FileEndOfFileInfo, &eofi, sizeof(eofi))) {
      unsn_copy_file_error();
    }
    /* The destination file was set to sparse when extending it; revert the
     * sparse status. */
    if (!out_is_sparse && !in_is_sparse) {
      unsn_copy_file_set_sparse(hout, FALSE);
    }
  }

  caml_release_runtime_system();
  BOOL res = DeviceIoControl(hout, FSCTL_DUPLICATE_EXTENTS_TO_FILE,
                             &dupl, sizeof(dupl), NULL, 0, NULL, NULL);
  caml_acquire_runtime_system();
  if (!res) {
    unsn_copy_file_error();
  } else {
    /* FSCTL_DUPLICATE_EXTENTS_TO_FILE does not advance the destination file
     * offset. */
    LARGE_INTEGER copied;
    copied.QuadPart = Long_val(len);
    if (!SetFilePointerEx(hout, copied, NULL, FILE_CURRENT)) {
      unsn_copy_file_error();
    }
  }

  CAMLreturn(len);
}

#else /* defined(__linux__) || defined(__FreeBSD__) || defined(__sun) || defined(_WIN32) */


CAMLprim value unison_copy_file(value in_fd, value out_fd, value in_offs, value len)
{
  CAMLparam4(in_fd, out_fd, in_offs, len);
  caml_unix_error(ENOSYS, "copy_file", Nothing);
  CAMLreturn(Val_long(0));
}


#endif /* defined(__linux__) || defined(__FreeBSD__) || defined (__sun) || defined(_WIN32) */
