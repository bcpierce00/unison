#define WINVER 0x0500

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <windows.h>
#include <fcntl.h>
#include <sys/stat.h>

//#include <stdio.h>

#define Nothing ((value) 0)

extern void win32_maperr (DWORD errcode);
extern void uerror (char * cmdname, value arg);
extern value win_alloc_handle (HANDLE h);

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

CAMLprim value win_findnext_short(value valh)
{
  WIN32_FIND_DATAW fileinfo;
  BOOL retcode;

  CAMLparam1(valh);
  CAMLlocal1(valname);

  do {
    retcode = FindNextFileW(Handle_val(valh), &fileinfo);
    if (!retcode) {
      DWORD err = GetLastError();
      if (err == ERROR_NO_MORE_FILES) {
        if (! FindClose(Handle_val(valh))) {
          win32_maperr(GetLastError());
          uerror("closedir", Nothing);
        }
        raise_end_of_file();
      } else {
        win32_maperr(err);
        uerror("readdir", Nothing);
      }
    }
    //    fwprintf (stderr, L"%d %s\n", fileinfo.cAlternateFileName[0], fileinfo.cAlternateFileName);
  } while (fileinfo.cAlternateFileName[0] == 0);

  valname = caml_alloc_tuple (2);
  Store_field (valname, 0, copy_wstring(fileinfo.cFileName));
  Store_field (valname, 1, copy_wstring(fileinfo.cAlternateFileName));

  CAMLreturn (valname);
}

CAMLprim value win_findfirst_short(value name)
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
  valh = win_alloc_handle(h);
  //fwprintf (stderr, L"%d %s\n", fileinfo.cAlternateFileName[0], fileinfo.cAlternateFileName);
  if (fileinfo.cAlternateFileName[0] != 0) {
    valname = caml_alloc_tuple (2);
    Store_field (valname, 0, copy_wstring(fileinfo.cFileName));
    Store_field (valname, 1, copy_wstring(fileinfo.cAlternateFileName));
  } else
    valname = win_findnext_short(valh);
  v = caml_alloc_tuple(2);
  Store_field(v,0,valname);
  Store_field(v,1,valh);
  CAMLreturn (v);
}
