/* Unison file synchronizer: src/fsmonitor/solaris/fen_stubs.c */
/* Copyright 2021, Benjamin C. Pierce

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

#include <errno.h>
#include <port.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>


/* We define the flags here rather than pass in from OCaml code
 * because they're constant and it reduces extra processing. */
#define EV_FLAGS_FOLLOW FILE_MODIFIED | FILE_ATTRIB
#define EV_FLAGS_NOFOLLOW EV_FLAGS_FOLLOW | FILE_NOFOLLOW


CAMLprim value unsn_port_create()
{
  CAMLparam0();

  int port = port_create();
  if (port == -1) {
    uerror("port_create", Nothing);
  }

  CAMLreturn(Val_int(port));
}

CAMLprim value unsn_port_close(value v)
{
  CAMLparam1(v);

  int status = close(Int_val(v));
  if (status == -1) {
    uerror("port_close", Nothing);
  }

  CAMLreturn(Val_unit);
}


static int ev_flag_table[] = {
  FILE_ACCESS, FILE_MODIFIED, FILE_ATTRIB,
  FILE_DELETE, FILE_RENAME_TO, FILE_RENAME_FROM,
  FILE_TRUNC,
  FILE_NOFOLLOW, UNMOUNTED, MOUNTEDOVER, 0
};

struct event_obj {
  int cookie;
  int prev_events;
  struct file_obj fo;
};

static struct event_obj * EvObj_val(value v)
{
  return (struct event_obj *) (v & ~1);
}

static value Val_EvObj(struct event_obj *x)
{
  assert(((uintptr_t) x & 1) == 0);
  return (value) x | 1;
}

static void free_eo(struct event_obj *eo)
{
  if (eo == NULL) return;

  free(eo->fo.fo_name);
  free(eo);
}

CAMLprim value unsn_free_event_object(value eo)
{
  CAMLparam1(eo);
  free_eo(EvObj_val(eo));
  CAMLreturn(Val_unit);
}

static int port_associate_aux(int port, struct event_obj *eo, int follow)
{
  eo->prev_events = 0;

  /* Due to the synchronous nature of the protocol between Unison and
   * fsmonitor, there is no need to pass in stat times to port_associate().
   * Passing in stat times allows detecting changes that have occurred
   * between the stat() call and port_associate(), by way of simple time
   * comparison.
   *
   * If stat times would be passed in to port_associate() then the call to
   * stat() must remain in the C stub. It has been detected that passing in
   * [struct stat] value from OCaml will report incorrect change events
   * because the times lose precision in the nanoseconds. (Not sure why it
   * happens, it really shouldn't.) */

  eo->fo.fo_atime.tv_sec = eo->fo.fo_atime.tv_nsec =
  eo->fo.fo_mtime.tv_sec = eo->fo.fo_mtime.tv_nsec =
  eo->fo.fo_ctime.tv_sec = eo->fo.fo_ctime.tv_nsec = 0;

  /* The event object must remain valid and allocated for the entire duration
   * the port association is valid. It must be freed only after an event is
   * received or after port_dissociate(). */

  return port_associate(port, PORT_SOURCE_FILE, (uintptr_t) &(eo->fo),
      !follow ? EV_FLAGS_NOFOLLOW : EV_FLAGS_FOLLOW, eo);
}

CAMLprim value unsn_port_associate(value port, value path, value follow, value cookie)
{
  CAMLparam4(port, path, follow, cookie);
  CAMLlocal1(result);

  struct event_obj *eo = malloc(sizeof(struct event_obj));
  if (eo == NULL) {
    unix_error(ENOMEM, "port_associate", path);
  }

  eo->cookie = Int_val(cookie);
  eo->fo.fo_name = strdup(String_val(path));

  int status = port_associate_aux(Int_val(port), eo, Bool_val(follow));
  if (status == -1) {
    free_eo(eo);
    uerror("port_associate", path);
  }

  /* Returning a malloc'ed pointer as a value is not fully safe as it's not
   * possible to restrict the value from being used in OCaml code after having
   * been freed (likewise, not possible to prevent double freeing).
   *
   * free_event_object() must be called after receiving an event (unless
   * re-associating) and after calling port_dissociate().
   *
   * A safer solution is to create a custom data block with a finalizer to
   * free the memory. The current implementation does not use this solution
   * to avoid the overhead. */
  CAMLreturn(Val_EvObj(eo));
}

CAMLprim value unsn_port_reassociate(value port, value eo_val, value follow)
{
  CAMLparam3(port, eo_val, follow);

  struct event_obj *eo = EvObj_val(eo_val);
  if (eo == NULL) {
    unix_error(EINVAL, "port_reassociate",
        caml_copy_string("NULL eo; this indicates a BUG!"));
  }

  if (eo->prev_events & FILE_EXCEPTION) {
    CAMLreturn(Val_false);
  }

  /* The object to associate is expected to exist, except when re-associating.
   * It may happen that the file or directory has already been deleted without
   * the delete event having been delivered. This may happen when recursively
   * deleting files and directories. The deletion of a file within a directory
   * will first produce a directory modification event, which also dissociates
   * the directory. Now, when the directory is subsequently deleted, the delete
   * event is lost before re-association takes place. For this reason, when
   * re-associating, ENOENT errors must not be raised as exceptions. */
  int status = port_associate_aux(Int_val(port), eo, Bool_val(follow));
  if (status == -1) {
    CAMLreturn(Val_false);
  }

  CAMLreturn(Val_true);
}

CAMLprim value unsn_port_dissociate(value port, value eo_val)
{
  CAMLparam2(port, eo_val);

  struct event_obj *eo = EvObj_val(eo_val);
  if (eo == NULL) {
    unix_error(EINVAL, "port_dissociate",
        caml_copy_string("NULL eo; this indicates a BUG!"));
  }

  int status = port_dissociate(Int_val(port), PORT_SOURCE_FILE, (uintptr_t) &(eo->fo));
  if (status == -1 && errno != ENOENT) {
    uerror("port_dissociate", caml_copy_string(eo->fo.fo_name));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value unsn_port_get(value port)
{
  CAMLparam1(port);
  CAMLlocal3(result, l, tmpl);
  int status;
  uint_t cnt = 0;
  port_event_t *pel;
  struct timespec timeout;

  timeout.tv_sec = timeout.tv_nsec = 0;

  result = Val_emptylist;

  status = port_getn(Int_val(port), NULL, 0, &cnt, &timeout);
  if (status == -1 && errno != ETIME && errno != EINTR) {
    fprintf(stderr, " unison-fsmonitor: [port_getn] Warning: error getting event count: %i\n", errno);
  }

  if (!cnt) {
    CAMLreturn(result);
  }

  pel = malloc(cnt * sizeof(port_event_t));
  if (pel == NULL) {
    unix_error(ENOMEM, "port_getn", Nothing);
  }

  status = port_getn(Int_val(port), pel, cnt, &cnt, &timeout);
  if (status == -1 && errno != ETIME && errno != EINTR) {
    free(pel);
    uerror("port_getn", Nothing);
  }

  for (int j = 0; j < cnt; j++) {
    if (pel[j].portev_source != PORT_SOURCE_FILE) {
      continue;
    }

    struct event_obj *eo = pel[j].portev_user;

    if (eo == NULL) {
      free(pel);
      unix_error(EINVAL, "portev_user",
          caml_copy_string("NULL eo; this indicates a BUG!"));
    }

    eo->prev_events = pel[j].portev_events;

    l = Val_emptylist;

    for (int i = 0; ev_flag_table[i]; i++) {
      if (!(pel[j].portev_events & ev_flag_table[i])) {
	continue;
      }

      tmpl = caml_alloc_small(2, Tag_cons);
      Field(tmpl, 0) = Val_int(i);
      Field(tmpl, 1) = l;
      l = tmpl;
    }

    tmpl = caml_alloc_small(4, 0);
    Field(tmpl, 0) = port;
    Field(tmpl, 1) = Val_EvObj(eo);
    Field(tmpl, 2) = Val_int(eo->cookie);
    Field(tmpl, 3) = l;
    l = tmpl;

    tmpl = caml_alloc_small(2, Tag_cons);
    Field(tmpl, 0) = l;
    Field(tmpl, 1) = result;
    result = tmpl;
  }

  free(pel);

  CAMLreturn(result);
}

