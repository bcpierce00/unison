#define WINVER 0x0500

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <windows.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdio.h>

#define NT_MAX_PATH 32768

#define Nothing ((value) 0)

struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  int crt_fd;
};
#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)

value copy_wstring(LPCWSTR s)
{
  int len;
  value res;

  len = 2 * wcslen(s) + 2;  /* NULL character included */
  res = caml_alloc_string(len);
  memmove((char *)String_val(res), s, len);
  return res;
}

extern void win32_maperr (DWORD errcode);
extern void uerror (char * cmdname, value arg);
extern value win_alloc_handle (HANDLE h);
extern value cst_to_constr (int n, int * tbl, int size, int deflt);

static int open_access_flags[12] = {
  GENERIC_READ, GENERIC_WRITE, GENERIC_READ|GENERIC_WRITE,
  0, 0, 0, 0, 0, 0, 0, 0, 0
};

static int open_create_flags[12] = {
  0, 0, 0, 0, O_APPEND, O_CREAT, O_TRUNC, O_EXCL, 0, 0, 0, 0
};

/****/

CAMLprim value win_rmdir(value path, value wpath)
{
  CAMLparam2(path, wpath);
  if (!RemoveDirectoryW((LPWSTR)String_val(wpath))) {
    win32_maperr (GetLastError ());
    uerror("rmdir", path);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_mkdir(value path, value wpath)
{
  CAMLparam2(path, wpath);
  if (!CreateDirectoryW((LPWSTR)String_val(wpath), NULL)) {
    win32_maperr (GetLastError ());
    uerror("mkdir", path);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_unlink(value path, value wpath)
{
  CAMLparam2(path, wpath);
  if (!DeleteFileW((LPWSTR)String_val(wpath))) {
    win32_maperr (GetLastError ());
    uerror("unlink", path);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_rename(value path1, value wpath1, value wpath2)
{
  int err, t;
  CAMLparam3(path1, wpath1, wpath2);

  t = 10;
 retry:
  if (!MoveFileExW((LPWSTR)String_val(wpath1), (LPWSTR)String_val(wpath2),
                  MOVEFILE_REPLACE_EXISTING)) {
    err = GetLastError ();
    if ((err == ERROR_SHARING_VIOLATION || err == ERROR_ACCESS_DENIED) &&
        t < 1000) {
      /* The renaming may fail due to an indexer or an anti-virus.
         We retry after a short time in the hope that this other
         program is done with the file. */
      Sleep (t);
      t *= 2;
      goto retry;
    }
    win32_maperr (err);
    uerror("rename", path1);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_link(value path1, value wpath1, value wpath2)
{
  CAMLparam3(path1, wpath1, wpath2);

  if (!CreateHardLinkW((LPWSTR)String_val(wpath2), (LPWSTR)String_val(wpath1),
                       NULL)) {
    win32_maperr (GetLastError ());
    uerror("rename", path1);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_chmod (value path, value wpath, value perm) {
  DWORD attr;
  CAMLparam3(path, wpath, perm);

  attr = GetFileAttributesW ((LPCWSTR)String_val (wpath));
  if (attr == INVALID_FILE_ATTRIBUTES) {
    win32_maperr (GetLastError ());
    uerror("chmod", path);
  }
  if (Int_val(perm) & _S_IWRITE)
    attr &= ~FILE_ATTRIBUTE_READONLY;
  else
    attr |= FILE_ATTRIBUTE_READONLY;

  if (!SetFileAttributesW ((LPCWSTR)String_val (wpath), attr)) {
    win32_maperr (GetLastError ());
    uerror("chmod", path);
  }

  CAMLreturn (Val_unit);
}

CAMLprim value win_utimes (value path, value wpath, value atime, value mtime) {
  HANDLE h;
  BOOL res;
  ULARGE_INTEGER iatime, imtime;
  FILETIME fatime, fmtime;

  CAMLparam4(path, wpath, atime, mtime);

  iatime.QuadPart = Double_val(atime);
  imtime.QuadPart = Double_val(mtime);

  /* http://www.filewatcher.com/p/Win32-UTCFileTime-1.44.tar.gz.93147/Win32-UTCFileTime-1.44/UTCFileTime.xs.html */
  /* http://savannah.nongnu.org/bugs/?22781#comment0 */
  if (iatime.QuadPart || imtime.QuadPart) {
    iatime.QuadPart += 11644473600ull;
    iatime.QuadPart *= 10000000ull;
    fatime.dwLowDateTime = iatime.LowPart;
    fatime.dwHighDateTime = iatime.HighPart;
    imtime.QuadPart += 11644473600ull;
    imtime.QuadPart *= 10000000ull;
    fmtime.dwLowDateTime = imtime.LowPart;
    fmtime.dwHighDateTime = imtime.HighPart;
  } else {
    GetSystemTimeAsFileTime (&fatime);
    fmtime = fatime;
  }
  h = CreateFileW ((LPWSTR) wpath, FILE_WRITE_ATTRIBUTES,
                   FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr (GetLastError ());
    uerror("utimes", path);
  }
  res = SetFileTime (h, NULL, &fatime, &fmtime);
  if (res == 0) {
    win32_maperr (GetLastError ());
    (void)CloseHandle (h);
    uerror("utimes", path);
  }
  res = CloseHandle (h);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("utimes", path);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_open (value path, value wpath, value flags, value perm) {
  int fileaccess, createflags, fileattrib, filecreate;
  SECURITY_ATTRIBUTES attr;
  HANDLE h;

  CAMLparam4 (path, wpath, flags, perm);

  fileaccess = convert_flag_list(flags, open_access_flags);

  createflags = convert_flag_list(flags, open_create_flags);
  if ((createflags & (O_CREAT | O_EXCL)) == (O_CREAT | O_EXCL))
    filecreate = CREATE_NEW;
  else if ((createflags & (O_CREAT | O_TRUNC)) == (O_CREAT | O_TRUNC))
    filecreate = CREATE_ALWAYS;
  else if (createflags & O_TRUNC)
    filecreate = TRUNCATE_EXISTING;
  else if (createflags & O_CREAT)
    filecreate = OPEN_ALWAYS;
  else
    filecreate = OPEN_EXISTING;

  if ((createflags & O_CREAT) && (Int_val(perm) & 0200) == 0)
    fileattrib = FILE_ATTRIBUTE_READONLY;
  else
    fileattrib = FILE_ATTRIBUTE_NORMAL;

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  h = CreateFileW((LPCWSTR) String_val(wpath), fileaccess,
                  FILE_SHARE_READ | FILE_SHARE_WRITE, &attr,
                  filecreate, fileattrib, NULL);

  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr (GetLastError ());
    uerror("open", path);
  }

  if (createflags & O_APPEND) SetFilePointer (h, 0, NULL, FILE_END);

  CAMLreturn(win_alloc_handle(h));
}


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

CAMLprim value win_has_correct_ctime()
{
  CAMLparam0();

  win_init();

  CAMLreturn (nt_api_available ? Val_true : Val_false);
}

#define MAKEDWORDLONG(a,b) ((DWORDLONG)(((DWORD)(a))|(((DWORDLONG)((DWORD)(b)))<<32)))
#define WINTIME_TO_TIME(t) (((ULONGLONG) t) / 10000000ull - 11644473600ull)
#define FILETIME_TO_TIME(ft) WINTIME_TO_TIME((((ULONGLONG) ft.dwHighDateTime) << 32) + ft.dwLowDateTime)
#define FILETIME_NT_TO_TIME(ft) WINTIME_TO_TIME(ft.QuadPart)

CAMLprim value win_stat(value path, value wpath, value lstat)
{
  uintnat dev;
  uintnat ino;
  uintnat kind;
  uintnat mode;
  uintnat nlink;
  uint64_t size;
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
  CAMLparam3(path,wpath, lstat);
  CAMLlocal1 (v);
  char *fname = Bool_val(lstat) ? "lstat" : "stat";

  win_init();

  h = CreateFileW ((LPCWSTR) String_val (wpath), FILE_READ_ATTRIBUTES,
                   FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL, OPEN_EXISTING,
                   FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_READONLY |
                   (Bool_val(lstat) ? FILE_FLAG_OPEN_REPARSE_POINT : 0), NULL);

  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr (GetLastError ());
    uerror(fname, path);
  }

  if (nt_api_available) {
    nt_status = pNtQueryInformationFile(h, &io_status, &file_info,
                                        sizeof file_info, FileAllInformation);

    /* Buffer overflow (a warning status code) is expected here. */
    if (NT_ERROR(nt_status)) {
      win32_maperr(pRtlNtStatusToDosError(nt_status));
      (void) CloseHandle(h);
      uerror(fname, path);
    }
  }

  res = GetFileInformationByHandle (h, &info);
  if (res == 0) {
    win32_maperr (GetLastError ());
    (void) CloseHandle (h);
    uerror(fname, path);
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
    win32_maperr (GetLastError ());
    uerror(fname, path);
  }

  if (Bool_val(lstat) && !syml) {
    CAMLreturn(win_stat(path, wpath, Val_false));
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
  Store_field(v, 8, copy_int64(size));
  Store_field(v, 9, copy_double(atime));
  Store_field(v, 10, copy_double(mtime));
  Store_field(v, 11, copy_double(ctime));

  CAMLreturn (v);
}

CAMLprim value win_chdir (value path, value wpath)
{
  CAMLparam2(path,wpath);
  if (!SetCurrentDirectoryW ((LPWSTR)wpath)) {
    win32_maperr(GetLastError());
    uerror("chdir", path);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_getcwd (value unit)
{
  int res;
  wchar_t s[NT_MAX_PATH];
  CAMLparam0();
  CAMLlocal1 (path);

  res = GetCurrentDirectoryW (NT_MAX_PATH, s);
  if (res == 0) {
    win32_maperr(GetLastError());
    uerror("getcwd", Nothing);
  }
  /* Normalize the path */
  res = GetLongPathNameW (s, s, NT_MAX_PATH);
  if (res == 0) {
    win32_maperr(GetLastError());
    uerror("getcwd", Nothing);
  }
  /* Convert the drive letter to uppercase */
  if (s[0] >= L'a' && s[0] <= L'z') s[0] -= 32;
  path = copy_wstring(s);
  CAMLreturn (path);
}

CAMLprim value win_findfirstw(value name)
{
  HANDLE h;
  WIN32_FIND_DATAW fileinfo;

  CAMLparam1(name);
  CAMLlocal3(v, valname, valh);

  h = FindFirstFileW((LPCWSTR) String_val(name),&fileinfo);
  if (h == INVALID_HANDLE_VALUE) {
    DWORD err = GetLastError();
    if ((err == ERROR_NO_MORE_FILES) || (err == ERROR_FILE_NOT_FOUND))
      raise_end_of_file();
    else {
      win32_maperr(err);
      uerror("opendir", Nothing);
    }
  }
  valname = copy_wstring(fileinfo.cFileName);
  valh = win_alloc_handle(h);
  v = alloc_small(2, 0);
  Field(v,0) = valname;
  Field(v,1) = valh;
  CAMLreturn (v);
}

CAMLprim value win_findnextw(value valh)
{
  WIN32_FIND_DATAW fileinfo;
  BOOL retcode;

  CAMLparam1(valh);

  retcode = FindNextFileW(Handle_val(valh), &fileinfo);
  if (!retcode) {
    DWORD err = GetLastError();
    if (err == ERROR_NO_MORE_FILES)
      raise_end_of_file();
    else {
      win32_maperr(err);
      uerror("readdir", Nothing);
    }
  }
  CAMLreturn (copy_wstring(fileinfo.cFileName));
}

CAMLprim value win_findclosew(value valh)
{
  CAMLparam1(valh);

  if (! FindClose(Handle_val(valh))) {
    win32_maperr(GetLastError());
    uerror("closedir", Nothing);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_getenv(value var)
{
  LPWSTR s;
  DWORD len;
  CAMLparam1(var);
  CAMLlocal1(res);

  s = stat_alloc (65536);

  len = GetEnvironmentVariableW((LPCWSTR) String_val(var), s, 65536);
  if (len == 0) { stat_free (s); raise_not_found(); }

  res = copy_wstring(s);
  stat_free (s);
  CAMLreturn (res);

}

CAMLprim value win_putenv(value var, value wvar, value v)
{
  BOOL res;
  CAMLparam3(var, wvar, v);

  res = SetEnvironmentVariableW((LPCWSTR) String_val(wvar), (LPCWSTR) v);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("putenv", var);
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_argv(value unit)
{
  int n, i;
  LPWSTR * l;

  CAMLparam0();
  CAMLlocal2(v,res);

  l = CommandLineToArgvW (GetCommandLineW (), &n);

  if (l == NULL) {
    win32_maperr (GetLastError ());
    uerror("argv", Nothing);
  }
  res = caml_alloc (n, 0);
  for (i = 0; i < n; i++) {
    v = copy_wstring (l[i]);
    Store_field (res, i, v);
  }
  LocalFree (l);
  CAMLreturn (res);
}

CAMLprim value w_create_process_native
(value prog, value wprog, value wargs, value fd1, value fd2, value fd3)
{
  int res, flags;
  PROCESS_INFORMATION pi;
  STARTUPINFOW si;
  wchar_t fullname [MAX_PATH];
  HANDLE h;
  CAMLparam5(wprog, wargs, fd1, fd2, fd3);

  res = SearchPathW (NULL, (LPCWSTR) String_val(wprog), L".exe",
                     MAX_PATH, fullname, NULL);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("create_process", prog);
  }

  ZeroMemory(&si, sizeof(STARTUPINFO));

  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = Handle_val(fd1);
  si.hStdOutput = Handle_val(fd2);
  si.hStdError = Handle_val(fd3);

  flags = GetPriorityClass (GetCurrentProcess ());
  /*
  h = CreateFile ("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
                  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h != INVALID_HANDLE_VALUE)
    CloseHandle (h);
  else {
    flags |= CREATE_NEW_CONSOLE;
    //    si.dwFlags |= STARTF_USESHOWWINDOW;
    //    si.wShowWindow = SW_MINIMIZE;
  }
  */

  res = CreateProcessW (fullname, (LPWSTR) String_val(wargs),
                        NULL, NULL, TRUE, flags,
                        NULL, NULL, &si, &pi);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("create_process", prog);
  }

  CloseHandle (pi.hThread);
  CAMLreturn (Val_long (pi.hProcess));
}

CAMLprim value w_create_process(value * argv, int argn)
{
  return w_create_process_native(argv[0], argv[1], argv[2],
                                 argv[3], argv[4], argv[5]);
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
      tmp = caml_alloc_small(1, 0);
      Store_field(tmp, 0, win_alloc_handle(in));
      Store_field(ret, 0, tmp);
    }
    if (!GetFileType(out_orig)) {
      tmp = caml_alloc_small(1, 0);
      Store_field(tmp, 0, win_alloc_handle(out));
      Store_field(ret, 1, tmp);
    }
    if (!GetFileType(err_orig)) {
      tmp = caml_alloc_small(1, 0);
      Store_field(tmp, 0, win_alloc_handle(err));
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
      win32_maperr (GetLastError ());
      uerror("init_conin", Nothing);
    }
  }
}

CAMLprim value win_get_console_mode (value unit)
{
  DWORD mode;
  BOOL res;

  init_conin ();

  res = GetConsoleMode (conin, &mode);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("get_console_mode", Nothing);
  }

  return (Val_int (mode));
}

CAMLprim value win_set_console_mode (value mode)
{
  BOOL res;

  init_conin ();

  res = SetConsoleMode (conin, Int_val(mode));
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("set_console_mode", Nothing);
  }
  return (Val_unit);
}

CAMLprim value win_get_console_output_cp (value unit) {
  return (Val_int (GetConsoleOutputCP ()));
}

CAMLprim value win_set_console_output_cp (value cp) {
  BOOL res;
  res = SetConsoleOutputCP (Int_val (cp));
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("set_console_cp", Nothing);
  }
  return (Val_unit);
}
