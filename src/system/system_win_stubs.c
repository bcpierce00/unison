#define WINVER 0x0500

#include <winsock2.h>
#include <windows.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/version.h>
#if OCAML_VERSION < 41300
#define CAML_INTERNALS /* was needed from OCaml 4.06 to 4.12 */
#endif
#include <caml/osdeps.h>

#if OCAML_VERSION_MAJOR < 5
#define caml_uerror uerror
#define caml_win32_maperr win32_maperr
#define caml_win32_alloc_handle win_alloc_handle
#endif


/* Parts of code in the following section are originally copied from libuv.
 *
 * libuv
 * Copyright Joyent, Inc. and other Node contributors. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
/* BEGIN section originally copied from libuv win/winapi.h */

typedef struct _IO_STATUS_BLOCK {
  union {
    NTSTATUS Status;
    PVOID Pointer;
  };
  ULONG_PTR Information;
} IO_STATUS_BLOCK, *PIO_STATUS_BLOCK;

typedef struct _FILE_BASIC_INFORMATION {
  LARGE_INTEGER CreationTime;
  LARGE_INTEGER LastAccessTime;
  LARGE_INTEGER LastWriteTime;
  LARGE_INTEGER ChangeTime;
  DWORD FileAttributes;
} FILE_BASIC_INFORMATION, *PFILE_BASIC_INFORMATION;

typedef struct _FILE_STANDARD_INFORMATION {
  LARGE_INTEGER AllocationSize;
  LARGE_INTEGER EndOfFile;
  ULONG         NumberOfLinks;
  BOOLEAN       DeletePending;
  BOOLEAN       Directory;
} FILE_STANDARD_INFORMATION, *PFILE_STANDARD_INFORMATION;

typedef struct _FILE_INTERNAL_INFORMATION {
  LARGE_INTEGER IndexNumber;
} FILE_INTERNAL_INFORMATION, *PFILE_INTERNAL_INFORMATION;

typedef struct _FILE_EA_INFORMATION {
  ULONG EaSize;
} FILE_EA_INFORMATION, *PFILE_EA_INFORMATION;

typedef struct _FILE_ACCESS_INFORMATION {
  ACCESS_MASK AccessFlags;
} FILE_ACCESS_INFORMATION, *PFILE_ACCESS_INFORMATION;

typedef struct _FILE_POSITION_INFORMATION {
  LARGE_INTEGER CurrentByteOffset;
} FILE_POSITION_INFORMATION, *PFILE_POSITION_INFORMATION;

typedef struct _FILE_MODE_INFORMATION {
  ULONG Mode;
} FILE_MODE_INFORMATION, *PFILE_MODE_INFORMATION;

typedef struct _FILE_ALIGNMENT_INFORMATION {
  ULONG AlignmentRequirement;
} FILE_ALIGNMENT_INFORMATION, *PFILE_ALIGNMENT_INFORMATION;

typedef struct _FILE_NAME_INFORMATION {
  ULONG FileNameLength;
  WCHAR FileName[1];
} FILE_NAME_INFORMATION, *PFILE_NAME_INFORMATION;

typedef struct _FILE_ALL_INFORMATION {
  FILE_BASIC_INFORMATION     BasicInformation;
  FILE_STANDARD_INFORMATION  StandardInformation;
  FILE_INTERNAL_INFORMATION  InternalInformation;
  FILE_EA_INFORMATION        EaInformation;
  FILE_ACCESS_INFORMATION    AccessInformation;
  FILE_POSITION_INFORMATION  PositionInformation;
  FILE_MODE_INFORMATION      ModeInformation;
  FILE_ALIGNMENT_INFORMATION AlignmentInformation;
  FILE_NAME_INFORMATION      NameInformation;
} FILE_ALL_INFORMATION, *PFILE_ALL_INFORMATION;

typedef enum _FILE_INFORMATION_CLASS {
  FileDirectoryInformation = 1,
  FileFullDirectoryInformation,
  FileBothDirectoryInformation,
  FileBasicInformation,
  FileStandardInformation,
  FileInternalInformation,
  FileEaInformation,
  FileAccessInformation,
  FileNameInformation,
  FileRenameInformation,
  FileLinkInformation,
  FileNamesInformation,
  FileDispositionInformation,
  FilePositionInformation,
  FileFullEaInformation,
  FileModeInformation,
  FileAlignmentInformation,
  FileAllInformation,
  FileAllocationInformation,
  FileEndOfFileInformation,
  FileAlternateNameInformation,
  FileStreamInformation,
  FilePipeInformation,
  FilePipeLocalInformation,
  FilePipeRemoteInformation,
  FileMailslotQueryInformation,
  FileMailslotSetInformation,
  FileCompressionInformation,
  FileObjectIdInformation,
  FileCompletionInformation,
  FileMoveClusterInformation,
  FileQuotaInformation,
  FileReparsePointInformation,
  FileNetworkOpenInformation,
  FileAttributeTagInformation,
  FileTrackingInformation,
  FileIdBothDirectoryInformation,
  FileIdFullDirectoryInformation,
  FileValidDataLengthInformation,
  FileShortNameInformation,
  FileIoCompletionNotificationInformation,
  FileIoStatusBlockRangeInformation,
  FileIoPriorityHintInformation,
  FileSfioReserveInformation,
  FileSfioVolumeInformation,
  FileHardLinkInformation,
  FileProcessIdsUsingFileInformation,
  FileNormalizedNameInformation,
  FileNetworkPhysicalNameInformation,
  FileIdGlobalTxDirectoryInformation,
  FileIsRemoteDeviceInformation,
  FileAttributeCacheInformation,
  FileNumaNodeInformation,
  FileStandardLinkInformation,
  FileRemoteProtocolInformation,
  FileMaximumInformation
} FILE_INFORMATION_CLASS, *PFILE_INFORMATION_CLASS;

#if !defined(OCAML_VERSION) || OCAML_VERSION < 40300 || OCAML_VERSION >= 41400

typedef struct _REPARSE_DATA_BUFFER {
  ULONG  ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union {
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG Flags;
      WCHAR PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR PathBuffer[1];
    } MountPointReparseBuffer;
    struct {
      UCHAR  DataBuffer[1];
    } GenericReparseBuffer;
    struct {
      ULONG StringCount;
      WCHAR StringList[1];
    } AppExecLinkReparseBuffer;
  };
} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;

#endif /* !OCAML_VERSION */

typedef NTSTATUS (NTAPI *sNtQueryInformationFile)
                 (HANDLE FileHandle,
                  PIO_STATUS_BLOCK IoStatusBlock,
                  PVOID FileInformation,
                  ULONG Length,
                  FILE_INFORMATION_CLASS FileInformationClass);

typedef ULONG (NTAPI *sRtlNtStatusToDosError)
              (NTSTATUS Status);

sNtQueryInformationFile pNtQueryInformationFile;

sRtlNtStatusToDosError pRtlNtStatusToDosError;

#ifndef NT_ERROR
#define NT_ERROR(status) ((((ULONG) (status)) >> 30) == 3)
#endif

/* END section originally copied from libuv win/winapi.h */

static int nt_init_done = 0;
static int nt_api_available = 0;

/* BEGIN section originally copied from libuv win/winapi.c */

void win_init()
{
  HMODULE ntdll_module;

  if (nt_init_done) return;

  nt_init_done = 1;

  ntdll_module = GetModuleHandleA("ntdll.dll");
  if (ntdll_module == NULL) {
    nt_api_available = 0;
    return;
  }

  pNtQueryInformationFile = (sNtQueryInformationFile) GetProcAddress(
      ntdll_module, "NtQueryInformationFile");
  if (pNtQueryInformationFile == NULL) {
    nt_api_available = 0;
    return;
  }

  pRtlNtStatusToDosError = (sRtlNtStatusToDosError) GetProcAddress(
      ntdll_module, "RtlNtStatusToDosError");
  if (pRtlNtStatusToDosError == NULL) {
    nt_api_available = 0;
    return;
  }

  nt_api_available = 1;
}

/* END section originally copied from libuv win/winapi.c */

CAMLprim value win_has_correct_ctime(value unit)
{
  CAMLparam0();

  win_init();

  CAMLreturn (nt_api_available ? Val_true : Val_false);
}

#define MAKEDWORDLONG(a,b) ((DWORDLONG)(((DWORD)(a))|(((DWORDLONG)((DWORD)(b)))<<32)))
#define WINTIME_TO_TIME(t) ((((ULONGLONG) t) - 116444736000000000ull) / 10000000ull)
#define FILETIME_TO_TIME(ft) WINTIME_TO_TIME((((ULONGLONG) ft.dwHighDateTime) << 32) + ft.dwLowDateTime)
#define FILETIME_NT_TO_TIME(ft) WINTIME_TO_TIME(ft.QuadPart)

CAMLprim value win_stat(value path, value lstat)
{
  uintnat dev;
  uintnat ino;
  uintnat kind;
  uintnat mode;
  uintnat nlink;
  uint64_t size = 0;
  double atime;
  double mtime;
  double ctime;
  int syml = 0;

  int res;
  NTSTATUS nt_status;
  HANDLE h;
  BY_HANDLE_FILE_INFORMATION info;
  IO_STATUS_BLOCK io_status;
  FILE_ALL_INFORMATION file_info;
  CAMLparam2(path, lstat);
  CAMLlocal1 (v);
  char *fname = Bool_val(lstat) ? "lstat" : "stat";

  win_init();

  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(path));

  h = CreateFileW (wpath, FILE_READ_ATTRIBUTES,
                   FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL, OPEN_EXISTING,
                   FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_READONLY |
                   (Bool_val(lstat) ? FILE_FLAG_OPEN_REPARSE_POINT : 0), NULL);
  caml_stat_free(wpath);

  if (h == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    caml_uerror(fname, path);
  }

  if (nt_api_available) {
    nt_status = pNtQueryInformationFile(h, &io_status, &file_info,
                                        sizeof file_info, FileAllInformation);

    /* Buffer overflow (a warning status code) is expected here. */
    if (NT_ERROR(nt_status)) {
      caml_win32_maperr(pRtlNtStatusToDosError(nt_status));
      (void) CloseHandle(h);
      caml_uerror(fname, path);
    }
  }

  res = GetFileInformationByHandle (h, &info);
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    (void) CloseHandle (h);
    caml_uerror(fname, path);
  }

  if (Bool_val(lstat) &&
        (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
    /* The following code is partially copied from OCaml sources,
     * LGPL 2.1,
     * Copyright David Allsopp, MetaStack Solutions Ltd. */
    char buffer[16384];
    DWORD read;

    if (DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0, buffer, 16384, &read, NULL)) {
      if (((REPARSE_DATA_BUFFER*)buffer)->ReparseTag == IO_REPARSE_TAG_SYMLINK) {
        syml = 1;
        size = ((REPARSE_DATA_BUFFER*)buffer)->SymbolicLinkReparseBuffer.SubstituteNameLength / 2;
      }
    }
  }

  res = CloseHandle (h);
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    caml_uerror(fname, path);
  }

  if (Bool_val(lstat) && !syml) {
    CAMLreturn(win_stat(path, Val_false));
  }

  dev = info.dwVolumeSerialNumber;

  if (nt_api_available) {
    /* Use the same hashing formula as the original code */
    ino = ((DWORDLONG)file_info.InternalInformation.IndexNumber.QuadPart) +
      155825701*((DWORDLONG)file_info.InternalInformation.IndexNumber.HighPart);

    kind = file_info.BasicInformation.FileAttributes & FILE_ATTRIBUTE_DIRECTORY ? 1: 0;

    mode = 0000444;
    if (!(file_info.BasicInformation.FileAttributes & FILE_ATTRIBUTE_READONLY))
      mode |= 0000222;
    if (file_info.BasicInformation.FileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      mode |= 0000111;

    nlink = file_info.StandardInformation.NumberOfLinks;
    if (!syml) {
      size = file_info.StandardInformation.EndOfFile.QuadPart;
    }
    atime = (double) FILETIME_NT_TO_TIME(file_info.BasicInformation.LastAccessTime);
    mtime = (double) FILETIME_NT_TO_TIME(file_info.BasicInformation.LastWriteTime);
    if (file_info.BasicInformation.ChangeTime.QuadPart != 0) {
      ctime = (double) FILETIME_NT_TO_TIME(file_info.BasicInformation.ChangeTime);
    } else {
      ctime = (double) FILETIME_NT_TO_TIME(file_info.BasicInformation.CreationTime);
    }
  } else {
    // Apparently, we cannot trust the inode number to be stable when
    // nFileIndexHigh is 0.
    if (info.nFileIndexHigh == 0) info.nFileIndexLow = 0;
    /* The ocaml code truncates inode numbers to 31 bits.  We hash the
       low and high parts in order to lose as little information as
       possible. */
    ino = MAKEDWORDLONG(info.nFileIndexLow,info.nFileIndexHigh)+155825701*((DWORDLONG)info.nFileIndexHigh);

    kind = info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ? 1: 0;

    mode = 0000444;
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      mode |= 0000111;
    if (!(info.dwFileAttributes & FILE_ATTRIBUTE_READONLY))
      mode |= 0000222;

    nlink = info.nNumberOfLinks;
    if (!syml) {
      size = MAKEDWORDLONG(info.nFileSizeLow,info.nFileSizeHigh);
    }
    atime = (double) FILETIME_TO_TIME(info.ftLastAccessTime);
    mtime = (double) FILETIME_TO_TIME(info.ftLastWriteTime);
    ctime = (double) FILETIME_TO_TIME(info.ftCreationTime);
  }

  if (syml) {
    kind = 4;
    mode |= 0000111 | 0000444;
  }

  v = caml_alloc (12, 0);
  Store_field(v, 0, Val_int(dev));
  Store_field(v, 1, Val_int(ino));
  Store_field(v, 2, Val_int(kind));
  Store_field(v, 3, Val_int(mode));
  Store_field(v, 4, Val_int(nlink));
  Store_field(v, 5, Val_int(0));
  Store_field(v, 6, Val_int(0));
  Store_field(v, 7, Val_int(0));
  Store_field(v, 8, caml_copy_int64(size));
  Store_field(v, 9, caml_copy_double(atime));
  Store_field(v, 10, caml_copy_double(mtime));
  Store_field(v, 11, caml_copy_double(ctime));

  CAMLreturn (v);
}

/****/

CAMLprim value win_init_console(value unit)
{
  CAMLparam0();
  CAMLlocal2(ret, tmp);
  HANDLE in, out, err, in_orig, out_orig, err_orig;
  FILE *ign;

  ret = caml_alloc_tuple(3);
  Store_field(ret, 0, Val_int(0));
  Store_field(ret, 1, Val_int(0));
  Store_field(ret, 2, Val_int(0));

  in_orig = (HANDLE) GetStdHandle(STD_INPUT_HANDLE);
  out_orig = (HANDLE) GetStdHandle(STD_OUTPUT_HANDLE);
  err_orig = (HANDLE) GetStdHandle(STD_ERROR_HANDLE);

  if (!GetFileType(out_orig) || !GetFileType(err_orig)) {
    AllocConsole();
    /* There's nothing we can do about an error, so we're not going to check.
     * Already having a console returns an error, which we want to ignore. */
    if (GetStdHandle(STD_ERROR_HANDLE) == NULL) {
      MessageBoxW(NULL, L"Unable to open a console where debugging output "
                         "will be sent. The program will most likely crash "
                         "when trying to produce debugging output.\n\n"
                         "If the problem persists then remove any \"debug\" "
                         "preferences from the profile, use the text UI or "
                         "redirect standard output and error.",
                  L"Error", MB_OK | MB_ICONWARNING);
    }

    /* Windows C runtime fds for stdin, stdout, stderr are not restored
     * automatically. */
    if (_fileno(stdin) < 0) freopen_s(&ign, "CONIN$", "r", stdin);
    if (_fileno(stdout) < 0) freopen_s(&ign, "CONOUT$", "w", stdout);
    if (_fileno(stderr) < 0) freopen_s(&ign, "CONOUT$", "w", stderr);

    /* AllocConsole() is supposed to init these handles. */
    in = (HANDLE) GetStdHandle(STD_INPUT_HANDLE);
    out = (HANDLE) GetStdHandle(STD_OUTPUT_HANDLE);
    err = (HANDLE) GetStdHandle(STD_ERROR_HANDLE);

    /* Return only handles that are not already redirected by user. */
    if (!GetFileType(in_orig)) {
      tmp = caml_alloc(1, 0);
      Store_field(tmp, 0, caml_win32_alloc_handle(in));
      Store_field(ret, 0, tmp);
    }
    if (!GetFileType(out_orig)) {
      tmp = caml_alloc(1, 0);
      Store_field(tmp, 0, caml_win32_alloc_handle(out));
      Store_field(ret, 1, tmp);
    }
    if (!GetFileType(err_orig)) {
      tmp = caml_alloc(1, 0);
      Store_field(tmp, 0, caml_win32_alloc_handle(err));
      Store_field(ret, 2, tmp);
    }
  }

  CAMLreturn(ret);
}

static HANDLE conin = INVALID_HANDLE_VALUE;

static void init_conin ()
{
  if (conin == INVALID_HANDLE_VALUE) {
    conin = CreateFile ("CONIN$", GENERIC_READ | GENERIC_WRITE,
                        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                        OPEN_EXISTING, 0, 0);
    if (conin == INVALID_HANDLE_VALUE) {
      caml_win32_maperr(GetLastError());
      caml_uerror("init_conin", Nothing);
    }
  }
}

CAMLprim value win_get_console_mode (value unit)
{
  CAMLparam0();
  DWORD mode;
  BOOL res;

  init_conin ();

  res = GetConsoleMode (conin, &mode);
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    caml_uerror("get_console_mode", Nothing);
  }

  CAMLreturn(Val_int(mode));
}

CAMLprim value win_set_console_mode (value mode)
{
  CAMLparam1(mode);
  BOOL res;

  init_conin ();

  res = SetConsoleMode (conin, Int_val(mode));
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    caml_uerror("set_console_mode", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value win_get_console_output_cp (value unit) {
  CAMLparam0();
  CAMLreturn(Val_int(GetConsoleOutputCP()));
}

CAMLprim value win_set_console_output_cp (value cp) {
  CAMLparam1(cp);
  BOOL res;
  res = SetConsoleOutputCP (Int_val (cp));
  if (res == 0) {
    caml_win32_maperr(GetLastError());
    caml_uerror("set_console_cp", Nothing);
  }
  CAMLreturn(Val_unit);
}
