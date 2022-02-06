/* Stub code for controlling terminals. */

#ifdef _WIN32

#define WINVER 0x0600
#define _WIN32_WINNT 0x0600

#ifndef UNICODE
#define UNICODE
#endif

#endif /* _WIN32 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>    // alloc_tuple
#include <caml/memory.h>   // Store_field
#include <caml/fail.h>     // failwith
#include <caml/unixsupport.h> // uerror, unix_error
#include <errno.h>         // ENOSYS

// openpty
#if defined(__linux)
#include <pty.h>
#define HAS_OPENPTY 1
#endif

#if defined(__APPLE__) || defined(__NetBSD__)
#include <util.h>
#define HAS_OPENPTY 1
#endif

#ifdef __FreeBSD__
#include <sys/types.h>
#include <libutil.h>
#define HAS_OPENPTY 1
#endif

#ifdef HAS_OPENPTY

#include <sys/ioctl.h>
#include <sys/types.h>

CAMLprim value setControllingTerminal(value fdVal) {
  CAMLparam1(fdVal);
  int fd = Int_val(fdVal);
  if (ioctl(fd, TIOCSCTTY, (char *) 0) < 0)
    uerror("ioctl", (value) 0);
  CAMLreturn(Val_unit);
}

/* c_openpty: unit -> (int * Unix.file_descr) */
CAMLprim value c_openpty(value unit) {
  CAMLparam0();
  CAMLlocal1(pair);
  int master, slave;
  if (openpty(&master,&slave,NULL,NULL,NULL) < 0)
    uerror("openpty", (value) 0);
  pair = caml_alloc_tuple(2);
  Store_field(pair,0,Val_int(master));
  Store_field(pair,1,Val_int(slave));
  CAMLreturn(pair);
}

#else // not HAS_OPENPTY

CAMLprim value setControllingTerminal(value fdVal) {
  unix_error (ENOSYS, "setControllingTerminal", Nothing);
}

CAMLprim value c_openpty(value unit) {
  unix_error (ENOSYS, "openpty", Nothing);
}

#endif

#ifdef _WIN32

#ifndef CAMLassert
#define CAMLassert(x) ((void) 0)
#endif

#include <windows.h>

extern void win32_maperr(DWORD errcode);
extern value win_alloc_handle(HANDLE h);

static int Twin_multi_byte_to_wide_char(const char *s, int slen,
                                        wchar_t *out, int outlen)
{
  int retcode;

  CAMLassert (s != NULL);

  if (slen == 0)
    return 0;

  retcode =
    MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                        s, slen, out, outlen);
  if (retcode == 0)
    retcode = MultiByteToWideChar(CP_ACP, 0, s, slen, out, outlen);

  if (retcode == 0) {
    win32_maperr(GetLastError());
    uerror("", Nothing);
  }

  return retcode;
}

#ifndef Bytes_val /* Hack to know that we are on OCaml < 4.06.
                     #include <caml/version.h> is not always found, for some reason. */
static void* caml_stat_alloc_noexc(asize_t sz)
{
  return malloc(sz);
}
#endif /* OCAML_VERSION < 40600 */

static wchar_t* Tcaml_stat_strdup_to_utf16(const char *s)
{
  wchar_t * ws;
  int retcode;

  retcode = Twin_multi_byte_to_wide_char(s, -1, NULL, 0);
  ws = caml_stat_alloc_noexc(retcode * sizeof(*ws));
  Twin_multi_byte_to_wide_char(s, -1, ws, retcode);

  return ws;
}

#ifndef Data_abstract_val /* OCaml < 4.05 */
#define Data_abstract_val(v) ((void*) Op_val(v))
#endif

#define PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE 0x00020016

typedef VOID* HPCON;

typedef HRESULT (WINAPI *sCreatePseudoConsole)
          (COORD size, HANDLE hInput, HANDLE hOutput, DWORD dwFlags, HPCON* phPC);

typedef void (WINAPI *sClosePseudoConsole) (HPCON hPC);

sCreatePseudoConsole pCreatePseudoConsole;
sClosePseudoConsole pClosePseudoConsole;

CAMLprim value win_openpty(value unit)
{
  CAMLparam0();
  CAMLlocal4(tup, tmp1, tmp2, tmp3);
  HPCON pty;
  HANDLE i1, i2, o1, o2;
  COORD size = { .X = 80, .Y = 25 };

  HMODULE kernel32_module = GetModuleHandleW(L"kernel32.dll");
  if (kernel32_module == NULL) {
    unix_error(ENOSYS, "openpty", Nothing);
  }

  /* This is the only way to use the new API while remaining compatible
   * with older Windows versions. */
  pCreatePseudoConsole = (sCreatePseudoConsole)
      GetProcAddress(kernel32_module, "CreatePseudoConsole");
  if (pCreatePseudoConsole == NULL) {
    unix_error(ENOSYS, "openpty", Nothing);
  }

  /* Read-write pipes don't seem to work well with PTY and cause deadlocks.
   * socketpair() is not supported by Windows and emulation code is just too
   * much to carry around (also, emulation by PF_INET sockets is a bit messy;
   * emulation by PF_UNIX sockets is only supported starting Windows 10 1803).
   * Simpler to use two separate pipes then. */
  if (!CreatePipe(&i1, &o1, NULL, 0)) {
    win32_maperr(GetLastError());
    uerror("openpty", Nothing);
  }
  if (!CreatePipe(&i2, &o2, NULL, 0)) {
    win32_maperr(GetLastError());
    if (o1 != INVALID_HANDLE_VALUE) CloseHandle(o1);
    if (i1 != INVALID_HANDLE_VALUE) CloseHandle(i1);
    uerror("openpty", Nothing);
  }

  if (pCreatePseudoConsole(size, i1, o2, 0, &pty) != S_OK) {
    win32_maperr(GetLastError());
    if (o1 != INVALID_HANDLE_VALUE) CloseHandle(o1);
    if (i1 != INVALID_HANDLE_VALUE) CloseHandle(i1);
    if (o2 != INVALID_HANDLE_VALUE) CloseHandle(o2);
    if (i2 != INVALID_HANDLE_VALUE) CloseHandle(i2);
    uerror("openpty", Nothing);
  }

  tmp1 = caml_alloc_tuple(2);
  Store_field(tmp1, 0, win_alloc_handle(i2));
  Store_field(tmp1, 1, win_alloc_handle(o1));

  tmp2 = caml_alloc(1, Abstract_tag);
  *((HPCON *) Data_abstract_val(tmp2)) = pty;

  tmp3 = caml_alloc_tuple(2);
  Store_field(tmp3, 0, win_alloc_handle(i1));
  Store_field(tmp3, 1, win_alloc_handle(o2));

  tup = caml_alloc_tuple(3);
  Store_field(tup, 0, tmp1);
  Store_field(tup, 1, tmp2);
  Store_field(tup, 2, tmp3);

  CAMLreturn(tup);
}

CAMLprim value win_closepty(value pty)
{
  CAMLparam1(pty);

  HMODULE kernel32_module = GetModuleHandleW(L"kernel32.dll");
  if (kernel32_module == NULL) {
    unix_error(ENOSYS, "closepty", Nothing);
  }

  /* This is the only way to use the new API while remaining compatible
   * with older Windows versions. */
  pClosePseudoConsole = (sClosePseudoConsole)
      GetProcAddress(kernel32_module, "ClosePseudoConsole");
  if (pClosePseudoConsole == NULL) {
    unix_error(ENOSYS, "closepty", Nothing);
  }

  pClosePseudoConsole(*((HPCON *) Data_abstract_val(pty)));

  CAMLreturn(Val_unit);
}

static void prepareSiWithPty(value prog, STARTUPINFOEX *si, HPCON pty)
{
#ifndef PROC_THREAD_ATTRIBUTE_HANDLE_LIST
  unix_error(ENOSYS, "create_process_pty", prog);
#else
  SIZE_T size;

  ZeroMemory(si, sizeof(*si));
  si->StartupInfo.cb = sizeof(STARTUPINFOEX);

  /* 2 attributes, as PROC_THREAD_ATTRIBUTE_HANDLE_LIST will be added later. */
  InitializeProcThreadAttributeList(NULL, 2, 0, &size);
  si->lpAttributeList = malloc(size);
  if (!si->lpAttributeList) {
    unix_error(ENOMEM, "create_process_pty", prog);
  }

  if (!InitializeProcThreadAttributeList(si->lpAttributeList, 2, 0, &size)) {
    win32_maperr(GetLastError());
    free(si->lpAttributeList);
    uerror("create_process_pty", prog);
  }

  if (!UpdateProcThreadAttribute(si->lpAttributeList, 0,
      PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE, pty, sizeof(pty), NULL, NULL)) {
    win32_maperr(GetLastError());
    DeleteProcThreadAttributeList(si->lpAttributeList);
    free(si->lpAttributeList);
    uerror("create_process_pty", prog);
  }
#endif
}

CAMLprim value w_create_process_pty_native
  (value prog, value args, value pty, value fd1, value fd2, value fd3)
{
  CAMLparam5(prog, args, pty, fd1, fd2);
  CAMLxparam1(fd3);
  int res, flags;
  BOOL err = FALSE;
  PROCESS_INFORMATION pi;
  STARTUPINFOEX si;
  wchar_t fullname[MAX_PATH];
  wchar_t *wprog, *wargs;
  HANDLE hp;
  HANDLE inherit[3];
  HPCON hpc = *((HPCON *) Data_abstract_val(pty));

#ifndef PROC_THREAD_ATTRIBUTE_HANDLE_LIST
  unix_error(ENOSYS, "create_process_pty", prog);
#else
  wprog = Tcaml_stat_strdup_to_utf16(String_val(prog));

  res = SearchPathW(NULL, wprog, L".exe", MAX_PATH, fullname, NULL);
  caml_stat_free(wprog);
  if (res == 0) {
    win32_maperr(GetLastError());
    uerror("create_process_pty", prog);
  }

  prepareSiWithPty(prog, &si, hpc);

  hp = GetCurrentProcess();
  if (!DuplicateHandle(hp, Handle_val(fd1), hp, &(si.StartupInfo.hStdInput),
      0, TRUE, DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    err = TRUE;
    goto clean1;
  }
  if (!DuplicateHandle(hp, Handle_val(fd2), hp, &(si.StartupInfo.hStdOutput),
      0, TRUE, DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    err = TRUE;
    goto clean2;
  }
  if (!DuplicateHandle(hp, Handle_val(fd3), hp, &(si.StartupInfo.hStdError),
      0, TRUE, DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    err = TRUE;
    goto clean3;
  }
  si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;

  inherit[0] = si.StartupInfo.hStdInput;
  inherit[1] = si.StartupInfo.hStdOutput;
  inherit[2] = si.StartupInfo.hStdError;
  if (!UpdateProcThreadAttribute(si.lpAttributeList, 0,
      PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherit, sizeof(HANDLE) * 3,
      NULL, NULL)) {
    win32_maperr(GetLastError());
    err = TRUE;
    goto clean4;
  }

  flags = GetPriorityClass(GetCurrentProcess());
  flags |= EXTENDED_STARTUPINFO_PRESENT;

  wargs = Tcaml_stat_strdup_to_utf16(String_val(args));
  res = CreateProcessW(fullname, wargs, NULL, NULL, TRUE, flags,
          NULL, NULL, &si.StartupInfo, &pi);
  caml_stat_free(wargs);
  if (res == 0) {
    win32_maperr(GetLastError());
    err = TRUE;
  }

clean4:
  CloseHandle(si.StartupInfo.hStdError);
clean3:
  CloseHandle(si.StartupInfo.hStdOutput);
clean2:
  CloseHandle(si.StartupInfo.hStdInput);
clean1:
  DeleteProcThreadAttributeList(si.lpAttributeList);
  free(si.lpAttributeList);

  if (err) {
    uerror("create_process_pty", prog);
  }

  CloseHandle(pi.hThread);
  CAMLreturn(Val_long(pi.hProcess));
#endif
}

CAMLprim value w_create_process_pty(value *argv, int argn)
{
  return w_create_process_pty_native(argv[0], argv[1], argv[2],
                                     argv[3], argv[4], argv[5]);
}

#else // not _WIN32

CAMLprim value w_create_process_pty_native
  (value prog, value args, value pty, value fd1, value fd2, value fd3)
{
  unix_error(ENOSYS, "create_process_pty", Nothing);
}

CAMLprim value w_create_process_pty(value *argv, int argn)
{
  unix_error(ENOSYS, "create_process_pty", Nothing);
}

CAMLprim value win_openpty()
{
  unix_error(ENOSYS, "openpty", Nothing);
}

CAMLprim value win_closepty(value pty)
{
  unix_error(ENOSYS, "closepty", Nothing);
}

#endif
