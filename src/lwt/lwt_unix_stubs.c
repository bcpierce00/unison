#include <winsock2.h>
#include <windows.h>
#include <errno.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>
#include <caml/version.h>
#if OCAML_VERSION < 41300
#define CAML_INTERNALS /* was needed from OCaml 4.06 to 4.12 */
#endif
#include <caml/osdeps.h>

#if OCAML_VERSION_MAJOR < 5
#define caml_unix_cloexec_p unix_cloexec_p
#define caml_unix_error_of_code unix_error_of_code
#define caml_uerror uerror
#define caml_win32_maperr win32_maperr
#define caml_win32_alloc_handle win_alloc_handle
#define caml_win32_alloc_socket win_alloc_socket
#endif

//#define D(x) x
#define D(x) while(0){}

typedef struct
{
  OVERLAPPED overlapped;
  long id;
  long action;
} completionData;

#define Array_data(a, i) (((char *) Caml_ba_data_val(a)) + Long_val(i))

#ifndef Bytes_val
#define Bytes_val(x) ((unsigned char *) Bp_val(x))
#endif

CAMLprim value ml_blit_bytes_to_buffer
(value s, value i, value a, value j, value l)
{
  CAMLparam5(s, i, a, j, l);
  unsigned char *src = Bytes_val(s) + Long_val(i);
  char *dest = Array_data(a, j);
  memcpy(dest, src, Long_val(l));
  CAMLreturn(Val_unit);
}

CAMLprim value ml_blit_string_to_buffer
(value s, value i, value a, value j, value l)
{
  return ml_blit_bytes_to_buffer(s, i, a, j, l);
}

CAMLprim value ml_blit_buffer_to_bytes
(value a, value i, value s, value j, value l)
{
  CAMLparam5(a, i, s, j, l);
  char *src = Array_data(a, i);
  unsigned char *dest = Bytes_val(s) + Long_val(j);
  memcpy(dest, src, Long_val(l));
  CAMLreturn(Val_unit);
}

/****/

#define READ 0
#define WRITE 1
#define READ_OVERLAPPED 2
#define WRITE_OVERLAPPED 3
#define READDIRECTORYCHANGES 4
static char * action_name[5] = {
  "read", "write", "read(overlapped)", "write(overlapped)",
  "ReadDirectoryChangesW"
};

static value completionCallback;

static void invoke_completion_callback
(long id, long len, long errCode, long action) {
  CAMLparam0();
  CAMLlocal2 (err, name);
  value args[4];
  err = Val_long(0);
  if (errCode != NO_ERROR) {
    len = -1;
    caml_win32_maperr(errCode);
    err = caml_unix_error_of_code(errno);
  }
  name = caml_copy_string(action_name[action]);
  D(printf("Action %s completed: id %ld -> len %ld / err %d (errCode %ld)\n",
           action_name[action], id, len, errno, errCode));
  args[0] = Val_long(id);
  args[1] = Val_long(len);
  args[2] = err;
  args[3] = name;
  caml_callbackN(completionCallback, 4, args);
  D(printf("Callback performed\n"));
  CAMLreturn0;
}

typedef struct {
  long id;
  long len;
  long errCode;
  long action; } completionInfo;

int compN = 0;
int complQueueSize = 0;
completionInfo * complQueue = NULL;

static void completion (long id, long len, long errCode, long action) {
  D(printf("Queueing action %s: id %ld -> len %ld / err %d (errCode %ld)\n",
           action_name[action], id, len, errno, errCode));
  if (compN + 1 > complQueueSize) {
    completionInfo * queue;
    int n = complQueueSize * 2 + 1;
    D(printf("Resizing queue to %d\n", n));
    queue = (completionInfo *) GlobalAlloc(GPTR, n * sizeof(completionInfo));
    if (complQueue != NULL)
      CopyMemory (queue, complQueue, complQueueSize * sizeof(completionInfo));
    complQueue = queue;
    complQueueSize = n;
  }
  complQueue[compN].id = id;
  complQueue[compN].len = len;
  complQueue[compN].errCode = errCode;
  complQueue[compN].action = action;
  compN++;
}

CAMLprim value get_queue (value unit) {
  CAMLparam1 (unit);
  int i;
  for (i = 0; i < compN; i++)
    invoke_completion_callback
      (complQueue[i].id, complQueue[i].len,
       complQueue[i].errCode, complQueue[i].action);
  compN = 0;
  CAMLreturn (Val_unit);
}

/****/

static HANDLE main_thread;

static DWORD CALLBACK helper_thread (void * param) {
  D(printf("Helper thread created\n"));
  while (1) SleepEx(INFINITE, TRUE);
  return 0;
}

static VOID CALLBACK exit_thread(ULONG_PTR param) {
  D(printf("Helper thread exiting\n"));
  ExitThread(0);
}

static HANDLE get_helper_thread (value threads, int kind) {
  HANDLE h = (HANDLE) Field(threads, kind);

  if (h != INVALID_HANDLE_VALUE) return h;

  h = CreateThread (NULL, 0, helper_thread, NULL, 0, NULL);
  if (h == NULL) {
    caml_win32_maperr(GetLastError());
    caml_uerror("createHelperThread", Nothing);
  }
  Field(threads, kind) = (value) h;
  return h;
}

static void kill_thread (HANDLE *h) {
  D(printf("Killing thread\n"));
  QueueUserAPC(exit_thread, *h, 0);
  CloseHandle(*h);
  *h = INVALID_HANDLE_VALUE;
}

CAMLprim value win_kill_threads (value fd) {
  CAMLparam1(fd);
  if (Field(fd, 1) != Val_long(0)) {
    kill_thread((HANDLE *) &Field(Field(fd, 1), READ));
    kill_thread((HANDLE *) &Field(Field(fd, 1), WRITE));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value win_wrap_fd (value fd) {
  CAMLparam1(fd);
  CAMLlocal2(th, res);
  D(printf("Wrapping file descriptor (sync)\n"));
  res = caml_alloc_tuple(2);
  Store_field(res, 0, fd);
  th = caml_alloc(2, Abstract_tag);
  Field(th, READ) = (value) INVALID_HANDLE_VALUE;
  Field(th, WRITE) = (value) INVALID_HANDLE_VALUE;
  Store_field(res, 1, th);
  CAMLreturn(res);
}

/****/

typedef struct {
  long action;
  long id;
  HANDLE fd;
  char * buffer;
  long len;
  long error;
} ioInfo;


static VOID CALLBACK thread_completion(ULONG_PTR param) {
  ioInfo * info = (ioInfo *) param;
  completion (info->id, info->len, info->error, info->action);
  GlobalFree (info);
}

static VOID CALLBACK perform_io_on_thread(ULONG_PTR param) {
  ioInfo * info = (ioInfo *) param;
  DWORD l;
  BOOL res;

  D(printf("Starting %s: id %ld, len %ld\n",
           action_name[info->action], info->id, info->len));

  res =
    (info->action == READ)?
    ReadFile(info->fd, info->buffer,info->len, &l, NULL):
    WriteFile(info->fd, info->buffer,info->len, &l, NULL);
  if (!res) {
    info->len = -1;
    info->error = GetLastError ();
  } else {
    info->len = l;
    info->error = NO_ERROR;
  }
  D(printf("Action %s done: id %ld -> len %ld / err %d (errCode %ld)\n",
           action_name[info->action],
           info->id, info->len, errno, info->error));
  QueueUserAPC(thread_completion, main_thread, param);
}

static void thread_io
(long action, long id, value threads, HANDLE h, char * buf, long len) {
  ioInfo * info = GlobalAlloc(GPTR, sizeof(ioInfo));
  if (info == NULL) {
    errno = ENOMEM;
    caml_uerror(action_name[action], Nothing);
  }

  info->action = action;
  info->id = id;
  info->fd = h;
  info->buffer = buf;
  info->len = len;

  h = get_helper_thread(threads, action);
  QueueUserAPC(perform_io_on_thread, h, (ULONG_PTR) info);
}

/****/

static void CALLBACK overlapped_completion
(DWORD errCode, DWORD len, LPOVERLAPPED overlapped) {
  completionData * d = (completionData * )overlapped;
  completion (d->id, len, errCode, d->action);
  GlobalFree (d);
}

static void overlapped_action(long action, long id,
                              HANDLE fd, char *buf, long len) {
  BOOL res;
  long err;
  completionData * d = GlobalAlloc(GPTR, sizeof(completionData));
  if (d == NULL) {
    errno = ENOMEM;
    caml_uerror(action_name[action], Nothing);
  }
  d->id = id;
  d->action = action;

  D(printf("Starting %s: id %ld, len %ld\n", action_name[action], id, len));
  res =
    (action == READ_OVERLAPPED)?
    ReadFileEx(fd, buf, len, &(d->overlapped), overlapped_completion):
    WriteFileEx(fd, buf, len, &(d->overlapped), overlapped_completion);

  if (!res) {
    err = GetLastError ();
    if (err != ERROR_IO_PENDING) {
      caml_win32_maperr(err);
  D(printf("Action %s failed: id %ld -> err %d (errCode %ld)\n",
           action_name[action], id, errno, err));
      caml_uerror("ReadFileEx", Nothing);
    }
  }
}

CAMLprim value win_wrap_overlapped (value fd) {
  CAMLparam1(fd);
  CAMLlocal1(res);
  D(printf("Wrapping file descriptor (async)\n"));
  res = caml_alloc_tuple(2);
  Store_field(res, 0, fd);
  Store_field(res, 1, Val_long(0));
  CAMLreturn(res);
}

/****/

#define Handle(fd) Handle_val(Field(fd, 0))

CAMLprim value win_read
(value fd, value buf, value ofs, value len, value id) {
  CAMLparam5(fd, buf, ofs, len, id);

  if (Field(fd, 1) == Val_long(0))
    overlapped_action (READ_OVERLAPPED, Long_val(id), Handle(fd),
                       Array_data(buf, ofs), Long_val(len));
  else
    thread_io (READ, Long_val(id), Field(fd, 1), Handle(fd),
               Array_data(buf, ofs), Long_val(len));
  CAMLreturn (Val_unit);
}

CAMLprim value win_write
(value fd, value buf, value ofs, value len, value id) {
  CAMLparam5(fd, buf, ofs, len, id);

  if (Field(fd, 1) == Val_long(0))
    overlapped_action (WRITE_OVERLAPPED, Long_val(id), Handle(fd),
                       Array_data(buf, ofs), Long_val(len));
  else
    thread_io (WRITE, Long_val(id), Field(fd, 1), Handle(fd),
               Array_data(buf, ofs), Long_val(len));
  CAMLreturn (Val_unit);
}

/*
#ifndef SO_UPDATE_CONNECT_CONTEXT
#define SO_UPDATE_CONNECT_CONTEXT 0x7010
#endif

static void after_connect (SOCKET s) {
  if (!setsockopt(s, SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT, NULL, 0)) {
    win32_maperr (GetLastError ());
    uerror("after_connect", Nothing);
  }
}
*/

static HANDLE events[MAXIMUM_WAIT_OBJECTS];
//static OVERLAPPED oData[MAXIMUM_WAIT_OBJECTS];

CAMLprim value win_register_wait (value socket, value kind) {
  CAMLparam2(socket, kind);
  HANDLE h;
  long mask;

  D(printf("Register: %lx, kind %ld\n", (long)(Socket_val(socket)), Long_val(kind)));
  h = CreateEvent(NULL, TRUE, FALSE, NULL);
  mask = (Long_val(kind) == 0) ? FD_CONNECT : FD_ACCEPT;
  if (WSAEventSelect(Socket_val(socket), h, mask) == SOCKET_ERROR) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("WSAEventSelect", Nothing);
  }

  CAMLreturn(caml_win32_alloc_handle(h));
}

CAMLprim value win_check_connection (value socket, value kind, value h) {
  CAMLparam3 (socket, kind, h);
  WSANETWORKEVENTS evs;
  int res, err;

  D(printf("Check connection... socket = %lx; h = %lx\n",
              (long)(Socket_val(socket)), Handle_val(h)));
  if (WSAEnumNetworkEvents(Socket_val(socket), NULL, &evs)) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("WSAEnumNetworkEvents", Nothing);
  }
  if (WSAEventSelect(Socket_val(socket), NULL, 0) == SOCKET_ERROR) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("WSAEventSelect", Nothing);
  }
  if (!CloseHandle(Handle_val(h))) {
    caml_win32_maperr(GetLastError());
    caml_uerror("CloseHandle", Nothing);
  }
  err =
    evs.iErrorCode[(Long_val(kind) == 0) ? FD_CONNECT_BIT : FD_ACCEPT_BIT];
  D(printf("Check connection: %ld, err %d\n", evs.lNetworkEvents, err));
  if (err != 0) {
    caml_win32_maperr(err);
    caml_uerror("check_connection", Nothing);
  }
  CAMLreturn (Val_unit);
}

static HANDLE dummyEvent;

CAMLprim value init_lwt (value callb) {
  CAMLparam1 (callb);
  //  GUID GuidConnectEx = WSAID_CONNECTEX;
  //  SOCKET s;
  //  DWORD l;
  int i;

  D(printf("Init...\n"));
  caml_register_global_root(&completionCallback);
  completionCallback = callb;

  dummyEvent = CreateEvent(NULL, TRUE, FALSE, NULL);  // Dummy event

  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (),
                   GetCurrentProcess (), &main_thread,
                   0, FALSE, DUPLICATE_SAME_ACCESS);

  /*
  s = socket(AF_INET, SOCK_STREAM, 0);
  if (s == INVALID_SOCKET) return Val_unit;
  WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER,
           &GuidConnectEx, sizeof(GuidConnectEx),
           &ConnectEx, sizeof(ConnectExPtr),
           &l, NULL, NULL);
  closesocket(s);
  */

  D(printf("Init done\n"));
  CAMLreturn (Val_long (MAXIMUM_WAIT_OBJECTS));
}

CAMLprim value win_wait (value timeout, value event_list) {
  CAMLparam2(timeout, event_list);
  DWORD t, t2;
  DWORD res;
  long ret, n;
  t = Long_val(timeout);
  if (t < 0) t = INFINITE;
  t2 = (compN > 0) ? 0 : t;
  for (n = 0; event_list != Val_emptylist; event_list = Field(event_list, 1))
    events[n++] = Handle_val(Field(event_list, 0));
  D(printf("Waiting: %ld events, timeout %ldms -> %ldms\n", n, t, t2));
  res =
    (n > 0) ?
    WaitForMultipleObjectsEx(n, events, FALSE, t, TRUE) :
    WaitForMultipleObjectsEx(1, &dummyEvent, FALSE, t, TRUE);
  D(printf("Done waiting\n"));
  if ((t != t2) && (res == WAIT_TIMEOUT)) res = WAIT_IO_COMPLETION;
  switch (res) {
  case WAIT_TIMEOUT:
    D(printf("Timeout\n"));
    ret = -1;
    break;
  case WAIT_IO_COMPLETION:
    D(printf("I/O completion\n"));
    ret = -2;
    break;
  case WAIT_FAILED:
    D(printf("Wait failed\n"));
    ret = 0;
    caml_win32_maperr(GetLastError());
    caml_uerror("WaitForMultipleObjectsEx", Nothing);
    break;
  default:
    ret = res;
    D(printf("Event: %ld\n", res));
    break;
  }
  get_queue (Val_unit);
  CAMLreturn (Val_long(ret));
}

static long pipeSerial;

value win_pipe(int cloexec, long readMode, long writeMode) {
  CAMLparam0();
  SECURITY_ATTRIBUTES attr;
  HANDLE readh, writeh;
  CHAR name[MAX_PATH];
  CAMLlocal3(readfd, writefd, res);

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = cloexec ? FALSE : TRUE;

  sprintf(name, "\\\\.\\Pipe\\UnisonAnonPipe.%08lx.%08lx",
             GetCurrentProcessId(), pipeSerial++);

  readh =
    CreateNamedPipeA
    (name, PIPE_ACCESS_INBOUND | readMode, PIPE_TYPE_BYTE | PIPE_WAIT,
     1, UNIX_BUFFER_SIZE, UNIX_BUFFER_SIZE, 0, &attr);

  if (readh == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    caml_uerror("CreateNamedPipe", Nothing);
    return FALSE;
  }

  writeh =
    CreateFileA
    (name, GENERIC_WRITE, 0, &attr, OPEN_EXISTING,
     FILE_ATTRIBUTE_NORMAL | writeMode, NULL);

  if (writeh == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    CloseHandle(readh);
    caml_uerror("CreateFile", Nothing);
    return FALSE;
  }

  readfd = caml_win32_alloc_handle(readh);
  writefd = caml_win32_alloc_handle(writeh);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = readfd;
  Field(res, 1) = writefd;
  CAMLreturn (res);
}

CAMLprim value win_pipe_in (value cloexec, value unit) {
  CAMLparam0();
  CAMLreturn (win_pipe (caml_unix_cloexec_p(cloexec), FILE_FLAG_OVERLAPPED, 0));
}

CAMLprim value win_pipe_out (value cloexec, value unit) {
  CAMLparam0();
  CAMLreturn (win_pipe (caml_unix_cloexec_p(cloexec), 0, FILE_FLAG_OVERLAPPED));
}

static int socket_domain_table[] = {
  PF_UNIX, PF_INET, PF_INET6
};

static int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

CAMLprim value win_socket (value cloexec, value domain, value type, value proto) {
  CAMLparam3(domain, type, proto);
  SOCKET s;

  s = WSASocket(socket_domain_table[Int_val(domain)],
                socket_type_table[Int_val(type)],
                Int_val(proto),
                NULL, 0, WSA_FLAG_OVERLAPPED);
  D(printf("Created socket %lx\n", (long)s));
  if (s == INVALID_SOCKET) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("WSASocket", Nothing);
  }
  /* Ignore errors */
  SetHandleInformation((HANDLE) s, HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);
  CAMLreturn(caml_win32_alloc_socket(s));
}

/*
#ifndef WSAID_CONNECTEX
#define WSAID_CONNECTEX \
        {0x25a207b9,0xddf3,0x4660,{0x8e,0xe9,0x76,0xe5,0x8c,0x74,0x06,0x3e}}
#endif

typedef BOOL (WINAPI *ConnectExPtr)(SOCKET, const struct sockaddr *, int, PVOID, DWORD, LPDWORD, LPOVERLAPPED);

static ConnectExPtr ConnectEx = NULL;

CAMLprim value win_connect (value socket, value address, value id) {
  CAMLparam3(socket, address, id);
  SOCKET s = Socket_val (socket);
  struct sockaddr addr;
  int addr_len;
  DWORD err;
  int i;

  if (ConnectEx == NULL) {
    errno = ENOSYS;
    uerror("ConnectEx", Nothing);
  }
  if (eventCount == MAXIMUM_WAIT_OBJECTS) {
    errno = EAGAIN;
    uerror("ConnectEx", Nothing);
  }
  i = free_list[eventCount];
  eventCount++;

  ZeroMemory(&(oData[i]), sizeof(OVERLAPPED));
  oData[i].hEvent = events[i];
  ids[i] = Long_val(id);
  sockets[i] = s;

  get_sockaddr(address, &addr, &addr_len);
  if (!ConnectEx(s, &addr, addr_len, NULL, 0, 0, &(oData[i]))) {
    err = WSAGetLastError ();
    if (err != ERROR_IO_PENDING) {
      win32_maperr(err);
      uerror("ConnectEx", Nothing);
    }
  } else
      after_connect(s);
  CAMLreturn (Val_unit);
}
*/

static int notify_filter_flags[8] = {
  FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
  FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
  FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_LAST_ACCESS,
  FILE_NOTIFY_CHANGE_CREATION, FILE_NOTIFY_CHANGE_SECURITY
};

CAMLprim value win_readdirtorychanges
(value fd_val, value buf_val, value recursive, value flags, value id_val) {
  CAMLparam5(fd_val, buf_val, recursive, flags, id_val);
  long id = Long_val(id_val);
  HANDLE fd = Handle_val(fd_val);
  char * buf = Array_data(buf_val, 0);
  long len = Caml_ba_array_val(buf_val)->dim[0];
  long action = READDIRECTORYCHANGES;
  BOOL res;
  long err;
  int notify_filter = caml_convert_flag_list(flags, notify_filter_flags);
  completionData * d = GlobalAlloc(GPTR, sizeof(completionData));
  if (d == NULL) {
    errno = ENOMEM;
    caml_uerror(action_name[action], Nothing);
  }
  d->id = id;
  d->action = action;

  D(printf("Starting %s: id %ld, len %ld\n", action_name[action], id, len));

  res = ReadDirectoryChangesW (fd, buf, len, Bool_val(recursive),
                               notify_filter, NULL, &(d->overlapped),
                               overlapped_completion);

  if (!res) {
    err = GetLastError ();
    if (err != ERROR_IO_PENDING) {
      caml_win32_maperr(err);
  D(printf("Action %s failed: id %ld -> err %d (errCode %ld)\n",
           action_name[action], id, errno, err));
      caml_uerror("ReadDirectoryChangesW", Nothing);
    }
  }
  CAMLreturn (Val_unit);
}

CAMLprim value win_parse_directory_changes (value buf_val) {
  CAMLparam1(buf_val);
  CAMLlocal4(lst, tmp, elt, filename);
  char * pos = Array_data(buf_val, 0);
  FILE_NOTIFY_INFORMATION * entry;
  wchar_t *namebuf;

  lst = Val_long(0);
  while (1) {
    entry = (FILE_NOTIFY_INFORMATION *)pos;
    namebuf = calloc(entry->FileNameLength + 2, 1);
    memmove(namebuf, entry->FileName, entry->FileNameLength);
    elt = caml_alloc_tuple(2);
    Store_field (elt, 0, caml_copy_string_of_utf16(namebuf));
    free(namebuf);
    Store_field (elt, 1, Val_long(entry->Action - 1));
    tmp = caml_alloc_tuple(2);
    Store_field (tmp, 0, elt);
    Store_field (tmp, 1, lst);
    lst = tmp;
    if (entry->NextEntryOffset == 0) break;
    pos += entry->NextEntryOffset;
  }
  CAMLreturn(lst);
}

CAMLprim value win_open_directory (value path) {
  CAMLparam1 (path);
  HANDLE h;
  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(path));

  h = CreateFileW(wpath,
                  FILE_LIST_DIRECTORY,
                  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                  NULL,
                  OPEN_EXISTING,
                  FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
                  NULL);
  caml_stat_free(wpath);
  if (h == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    caml_uerror("open", path);
  }
  CAMLreturn(caml_win32_alloc_handle(h));
}

CAMLprim value win_long_path_name(value path) {
  CAMLparam1(path);
  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(path));
  wchar_t lbuf[32768] = L"";
  DWORD res;

  res = GetLongPathNameW(wpath, lbuf, 32768);
  caml_stat_free(wpath);

  CAMLreturn(res == 0 || res > 32767 ? path : caml_copy_string_of_utf16(lbuf));
}
