#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <wtypes.h>
#include <winbase.h>
#include <fcntl.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <share.h>
#include <errno.h>
#include <utime.h>
#include <wchar.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <direct.h>
#include <stdio.h>
#include <windows.h>

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

static value copy_wstring(LPCWSTR s)
{
  int len;
  value res;

  len = 2 * wcslen(s) + 2;  /* NULL character included */
  res = caml_alloc_string(len);
  memmove(String_val(res), s, len);
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
  0, 0, 0, 0, 0, O_CREAT, O_TRUNC, O_EXCL, 0, 0, 0, 0
};

static int open_flag_table[12] = {
  _O_RDONLY, _O_WRONLY, _O_RDWR, 0, _O_APPEND, _O_CREAT, _O_TRUNC,
  _O_EXCL, 0, 0, 0, 0
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
  CAMLparam3(path1, wpath1, wpath2);
  if (!MoveFileExW((LPWSTR)String_val(wpath1), (LPWSTR)String_val(wpath2),
		  MOVEFILE_REPLACE_EXISTING)) {
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

  CAMLreturn(win_alloc_handle(h));
}

/*
static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, 0, S_IFIFO, 0
};

static value stat_aux(int use_64, struct _stati64 *buf)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

  v = caml_alloc (12, 0);
  Store_field (v, 0, Val_int (buf->st_dev));
  Store_field (v, 1, Val_int (buf->st_ino));
  Store_field (v, 2, cst_to_constr (buf->st_mode & S_IFMT, file_kind_table,
                                    sizeof(file_kind_table) / sizeof(int), 0));
  Store_field (v, 3, Val_int(buf->st_mode & 07777));
  Store_field (v, 4, Val_int (buf->st_nlink));
  Store_field (v, 5, Val_int (buf->st_uid));
  Store_field (v, 6, Val_int (buf->st_gid));
  Store_field (v, 7, Val_int (buf->st_rdev));
  Store_field (v, 8,
               use_64 ? copy_int64(buf->st_size) : Val_int (buf->st_size));
  Store_field (v, 9, copy_double((double) buf->st_atime));
  Store_field (v, 10, copy_double((double) buf->st_mtime));
  Store_field (v, 11, copy_double((double) buf->st_ctime));
  CAMLreturn (v);
}
*/
#define MAKEDWORDLONG(a,b) ((DWORDLONG)(((DWORD)(a))|(((DWORDLONG)((DWORD)(b)))<<32)))
#define FILETIME_TO_TIME(ft) (((((ULONGLONG) ft.dwHighDateTime) << 32) + ft.dwLowDateTime) / 10000000ull - 11644473600ull)

CAMLprim value win_stat(value path, value wpath)
{
  int res, mode;
  HANDLE h;
  BY_HANDLE_FILE_INFORMATION info;
  CAMLparam2(path,wpath);
  CAMLlocal1 (v);

  h = CreateFileW ((LPCWSTR) String_val (wpath), 0, 0, NULL, OPEN_EXISTING,
		   FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_READONLY, NULL);
  
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr (GetLastError ());
    uerror("stat", path);
  }

  res = GetFileInformationByHandle (h, &info);
  if (res == 0) {
    win32_maperr (GetLastError ());
    (void) CloseHandle (h);
    uerror("stat", path);
  }

  res = CloseHandle (h);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("stat", path);
  }

  v = caml_alloc (12, 0);
  Store_field (v, 0, Val_int (info.dwVolumeSerialNumber));
  Store_field
    (v, 1, Val_int (MAKEDWORDLONG(info.nFileIndexLow,info.nFileIndexHigh)));
  Store_field
    (v, 2, Val_int (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY
		    ? 1: 0));
  mode = 0000444;
  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    mode |= 0000111;
  if (!(info.dwFileAttributes & FILE_ATTRIBUTE_READONLY))
    mode |= 0000222;
  Store_field (v, 3, Val_int(mode));
  Store_field (v, 4, Val_int (1));
  Store_field (v, 5, Val_int (0));
  Store_field (v, 6, Val_int (0));
  Store_field (v, 7, Val_int (0));
  Store_field
    (v, 8, copy_int64(MAKEDWORDLONG(info.nFileSizeLow,info.nFileSizeHigh)));
  Store_field
    (v, 9, copy_double((double) FILETIME_TO_TIME(info.ftLastAccessTime)));
  Store_field
    (v, 10, copy_double((double) FILETIME_TO_TIME(info.ftLastWriteTime)));
  Store_field
    (v, 11, copy_double((double) FILETIME_TO_TIME(info.ftCreationTime)));

  CAMLreturn (v);
}

/*
CAMLprim value win_stat(value path, value wpath)
{
  CAMLparam2(path,wpath);
  int ret;
  struct _stati64 buf;
  ret = _wstati64((const wchar_t *)String_val(wpath), &buf);
  if (ret == -1) uerror("stat", path);
  CAMLreturn(stat_aux(1, &buf));
}
*/

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
  int len;
  LPWSTR s;
  CAMLparam0();
  CAMLlocal1 (res);

  len = GetCurrentDirectoryW (0, NULL);
  if (len == 0) {
    win32_maperr(GetLastError());
    uerror("getcwd", Nothing);
  }
  s = stat_alloc (len * 2 + 2);
  len = GetCurrentDirectoryW (len, s);
  if (len == 0) {
    stat_free (s);
    win32_maperr(GetLastError());
    uerror("getcwd", Nothing);
  }
  res = copy_wstring(s);
  stat_free (s);
  CAMLreturn (res);
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
    if (err == ERROR_NO_MORE_FILES)
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
