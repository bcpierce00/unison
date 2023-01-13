/* Unison file synchronizer: src/props_acl.c */
/* Copyright 2020-2022, Tõivo Leedjärv

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

/* Supporting POSIX draft ACLs is not a goal, but may incidentally work
 * on some platforms. Only NFSv4 ACLs and Windows ACLs are intended to be
 * supported.
 *
 * On Solarish, both NFSv4 ACLs and POSIX draft ACLs are supported.
 * There is even support for cross-synchronizing between NFSv4 and
 * POSIX draft ACLs, but this support is currently disabled in props.ml
 * by checking if the resulting ACL matches the requested ACL (the check
 * fails with cross-synchronization).
 *
 * On FreeBSD and NetBSD, NFSv4 ACLs are supported. There is only limited
 * support for synchronizing POSIX draft ACLs (no default ACLs).
 *
 * On Darwin, extended ACLs are supported.
 *
 * On Windows, NTFS ACLs are supported via SDDL format. Only explicit
 * ACEs are synchronized, ignoring inherited ACEs completely. Users and
 * groups are represented as SID strings in SDDL, not as names.
 */

/* The external interface is defined as follows. Every supported platform
 * must implement this interface. ACL format can be platform-specific,
 * which will prevent cross-platform synchronization but still allows
 * synchronization within the platform.
 *
 *
 * SET the ACL
 * ===========
 * unit unison_acl_from_text(String path, String acl)
 *
 *   Set the requested ACL on the requested file or directory. The ACL
 *   must be in the same format as that returned by unison_acl_to_text().
 *   Empty string ACL means <no ACL> and results in removal of any
 *   existing ACL on the requested file or directory.
 *   Symbolic links are followed.
 *
 * Input parameters
 *   path - absolute path of a file or directory
 *   acl  - text representation of ACL to set on the path
 *
 * Return value
 *   No return value.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to set/remove ACL
 *     ACL not supported
 *     Error setting ACL
 *     Error removing ACL
 *     Error converting ACL from text
 *
 *
 * GET the ACL
 * ===========
 * String unison_acl_to_text(String path)
 *
 *   Get the current ACL on the requested file or directory. The ACL
 *   must be returned as a stable and deterministic text representation
 *   that meets the following criteria:
 *     - with multiple requests on the same file, the representation is
 *       always the same, unless the underlying ACL changes;
 *     - the same ACL on different files has the same representation.
 *   Symbolic links are followed.
 *
 * Input parameters
 *   path - absolute path of a file or directory
 *
 * Return value
 *   The text representation of the ACL;
 *   or the value of macro UNSN_ACL_EMPTY (or empty string "") meaning
 *     <no ACL> (or only trivial ACL)
 *   or the value of macro UNSN_ACL_NOT_SUPPORTED (currently "-1") if
 *     ACL is not supported on the requested path.
 *
 * Exceptions
 *   Failure MUST be raised when:
 *     Can't access file to get ACL
 *   Failure MAY voluntarily be raised for example when:
 *     Error getting ACL
 *     Error converting ACL to text
 *   If Failure is not raised on some error condition then an empty
 *   string "" MUST NOT be returned under any circumstances; return
 *   UNSN_ACL_NOT_SUPPORTED instead.
 *
 *
 * ===========
 * Definition of ACL format
 *
 * The format of ACL text representation is completely free as long as
 * following constraints are met:
 *   - output of unison_acl_to_text() can be used as
 *     input to unison_acl_from_text()
 *   - ACL synchronization is done only on the same platform.
 *
 * If ACLs must be synchronized between different platforms then the
 * currently used universal ACL format matches the definition from
 * illumos acl(5) man page [https://illumos.org/man/5/acl]. This applies
 * to both POSIX draft ACLs and NFSv4 ACLs. See the note on cross-platform
 * synchronization below.
 *
 * ACL is always in the form
 *
 *   acl_entry[,acl_entry]...
 *
 * Each acl_entry may be suffixed with a colon and userid/groupid.
 *
 * Examples:
 *
 *   POSIX draft ACL
 *
 *     user:tom:rw-,mask:rwx,group:staff:r-x:450
 *
 *   NFSv4 ACL
 *
 *        user:lp:rw------------:------I:allow:1300,
 *         owner@:--x-----------:------I:deny,
 *         owner@:rw-p---A-W-Co-:-------:allow,
 *     user:marks:r-------------:------I:deny:1270,
 *         group@:r-------------:-------:allow,
 *      everyone@:r-----a-R-c--s:-------:allow
 *
 *     (note that the example is folded, but it should actually be
 *     returned as one string line without newlines)
 *
 *
 * ===========
 * On cross-platform synchronization
 *
 * Currently there is no canonical ACL representation created specifically
 * for Unison. Existing platform APIs are used as much as possible, without
 * custom formatting and parsing.
 * A specific Unison ACL format could be truly common across platforms.
 *
 * If extended ACL synchronization capability is desired in the future then
 * it is only required to change the output of unison_acl_to_text() and the
 * input parsing in unison_acl_from_text().
 * The Unison archive format will not have to be changed as long as the
 * entire ACL and any eventual metadata is encoded within one string.
 * It is neither necessary to change the ACL code in props.ml.
 *
 * The issues of such cross-platform (e.g. between Windows and Unix-like)
 * synchronization lie not in the representation format, though. It is easy
 * enough to interpret the permission sets of NFSv4 and Windows ACLs in a
 * similar, equivalent way. It can be much more difficult to interpret the
 * subjects (users and groups) in a meaningful way. Purely for
 * synchronization, this can still work on some platforms, e.g. Solaris,
 * which allow the use of SIDs in ACL definition.
 */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>


#if defined(sun) || defined(__sun)  /* Solarish, all illumos-based OS,   */
#define __Solaris__                 /* OpenIndiana, OmniOS, SmartOS, ... */
#endif

/* Primitive check only, without explicitly checking for
 * POSIX or NFSv4. NFSv4-style ACLs are expected
 * but POSIX draft ACLs may work to some extent. */
#undef UNSN_HAS_FS_ACL
#if defined(__Solaris__) || defined(__FreeBSD__) || defined(__APPLE__)
#define UNSN_HAS_FS_ACL
#endif

#if defined(__NetBSD__)
#include <unistd.h>
#if defined(_PC_ACL_NFS4)
#define UNSN_HAS_FS_ACL
#endif
#endif

#if defined(_WIN32)
#define UNSN_HAS_FS_ACL
#endif


#define UNSN_ACL_NOT_SUPPORTED caml_copy_string("-1")


#ifndef UNSN_HAS_FS_ACL

CAMLprim value unison_acl_from_text(value path, value acl)
{
  CAMLparam0();
  CAMLreturn(Val_unit);
}

CAMLprim value unison_acl_to_text(value path)
{
  CAMLparam0();
  CAMLreturn(UNSN_ACL_NOT_SUPPORTED);
}

#else


#define UNSN_ACL_EMPTY caml_copy_string("")


#if defined(_WIN32)

/*#define ACL_DEBUG*/

#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif

#include <windows.h>
#include <aclapi.h>
#include <sddl.h>
#include <strsafe.h>

#include <caml/version.h>
#if OCAML_VERSION < 41300
#define CAML_INTERNALS /* was needed from OCaml 4.06 to 4.12 */
#endif
#include <caml/osdeps.h>

#ifdef ACL_DEBUG
#include <stdio.h>
#endif

static void unsn_acl_fail(char *msg, DWORD err)
{
  DWORD flags;
  char *sys_msg;
  DWORD sys_len;
  char fail_msg[160];
  const size_t LEN = sizeof(fail_msg) / sizeof(fail_msg[0]);

  flags =
    FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS;

  sys_len = FormatMessageA(flags, NULL, err, 0, (char *) &sys_msg, 0, NULL);
  if (!sys_len) {
    StringCbPrintfA(fail_msg, LEN, "%s (Windows error code: %d)", msg, err);
  } else {
    /* Assume last 3 characters are ".\r\n" (doesn't matter if they aren't),
     * and remove them. */
    if (sys_len > 3) {
      sys_msg[sys_len - 3] = '\0';
    }

    StringCbPrintfA(fail_msg, LEN,
      "%s (Windows error code: %d) %s", msg, err, sys_msg);
    LocalFree(sys_msg);
  }

  caml_failwith(fail_msg);
}

CAMLprim value unison_acl_from_text(value path, value acl)
{
  CAMLparam2(path, acl);
  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(path));
  wchar_t *wacl = caml_stat_strdup_to_utf16(String_val(acl));
  PCWSTR acl_text;
  PSECURITY_DESCRIPTOR sd;
  SECURITY_DESCRIPTOR_CONTROL sdc;
  DWORD sdc_rev;
  PSID owner = NULL, group = NULL;
  PACL DACL;
  BOOL DACLpresent = FALSE, isDef;
  BOOL ok = TRUE;
  SECURITY_INFORMATION si = 0;
  DWORD res;

#ifdef ACL_DEBUG
  printf_s(" ===> Setting ACL for |%ls|\n", wpath);
  printf_s(" ---> Input ACL value   |%ls|\n", wacl);
#endif

  if (wcslen(wacl) == 0) {
    acl_text = L"D:"; /* SDDL representation of empty ACL */
  } else {
    acl_text = wacl;
  }
#ifdef ACL_DEBUG
  printf_s(" ---> Setting ACL value |%ls|\n", acl_text);
#endif

  if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(acl_text,
         SDDL_REVISION_1, &sd, NULL)) {
    caml_stat_free(wpath);
    caml_stat_free(wacl);
    unsn_acl_fail("Error converting ACL from text", GetLastError());
  }

  caml_stat_free(wacl);

  ok = ok && GetSecurityDescriptorDacl(sd, &DACLpresent, &DACL, &isDef);
  ok = ok && GetSecurityDescriptorControl(sd, &sdc, &sdc_rev);

  if (!ok || !DACLpresent) {
    LocalFree(sd);
    caml_stat_free(wpath);

    caml_failwith("Error converting ACL from text (no ACL info present?)");
  }

  si |= DACL_SECURITY_INFORMATION;

  if (sdc & SE_DACL_PROTECTED) {
    si |= PROTECTED_DACL_SECURITY_INFORMATION;
  } else {
    si |= UNPROTECTED_DACL_SECURITY_INFORMATION;
  }

  res = SetNamedSecurityInfoW(wpath, SE_FILE_OBJECT,
          si, owner, group, DACL, NULL);

  LocalFree(sd);
  caml_stat_free(wpath);

  if (res == ERROR_ACCESS_DENIED) {
    caml_failwith("Error setting ACL: access denied. The process may require "
      "Administrator or \"Restore files\" privileges to set the ACL");
  }

  if (res != ERROR_SUCCESS) {
    unsn_acl_fail("Error setting ACL", res);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value unison_acl_to_text(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(path));
  int i, aceCnt;
  PWSTR acl_text;
  PSECURITY_DESCRIPTOR sd;
  SECURITY_DESCRIPTOR_CONTROL sdc;
  DWORD sdc_rev;
  PSID owner, group;
  PACL DACL;
  ACL_SIZE_INFORMATION aclInfo;
  PVOID ace;
  SECURITY_INFORMATION si = DACL_SECURITY_INFORMATION;
  DWORD res1, err;
  BOOL res2;

#ifdef ACL_DEBUG
  printf_s(" ===> Getting ACL for %ls\n", wpath);
#endif

  res1 = GetNamedSecurityInfoW(wpath, SE_FILE_OBJECT, si,
           &owner, &group, &DACL, NULL, &sd);
  caml_stat_free(wpath);

  if (res1 != ERROR_SUCCESS || sd == NULL) {
    unsn_acl_fail("Error getting ACL", res1);
  }

#ifdef ACL_DEBUG
  res2 = ConvertSecurityDescriptorToStringSecurityDescriptorW(sd,
           SDDL_REVISION_1, si, &acl_text, NULL);

  if (acl_text != NULL) {
    printf_s(" ---> Initial ACL text representation: %ls\n", acl_text);

    LocalFree(acl_text);
  }
#endif /* ACL_DEBUG */

  if (DACL == NULL) {
    LocalFree(sd);

#ifdef ACL_DEBUG
    printf_s(" ---> ACL not supported\n");
#endif
    CAMLreturn(UNSN_ACL_NOT_SUPPORTED);
  }

  if (!GetAclInformation(DACL, &aclInfo, sizeof(aclInfo), AclSizeInformation)) {
    LocalFree(sd);
    unsn_acl_fail("Error getting ACL information", GetLastError());
  }
  aceCnt = aclInfo.AceCount;

  /* Remove all inherited ACEs -- those cannot be restored in the other
   * replica, they are inherited from the parent directory. */
  for (i = aclInfo.AceCount - 1; i >= 0; i--) {
    if (!GetAce(DACL, i, &ace)) {
#ifdef ACL_DEBUG
      printf_s("GetAce failed (Windows error code %d)\n", GetLastError());
#endif
    } else if (((PACE_HEADER) ace)->AceFlags & INHERITED_ACE) {
      if (!DeleteAce(DACL, i)) {
#ifdef ACL_DEBUG
        printf_s("DeleteAce failed (Windows error code %d)\n", GetLastError());
#endif
      } else {
        aceCnt--;
      }
    }
  }

  /* Even when individual inherited ACEs have been removed, the entire ACL
   * may have been marked as AUTO_INHERITED. Remove this flag to make
   * synchronization paranoid checks more reliable. It is unknown if it
   * can cause synchronization failures, but it doesn't matter - inherited
   * ACLs can't be propagated in any case. */
  if (!SetSecurityDescriptorControl(sd, SE_DACL_AUTO_INHERITED, 0)) {
#ifdef ACL_DEBUG
    unsn_acl_fail("Error in ACL control information", GetLastError());
#endif
  }

  if (aceCnt == 0) { /* No explicit entries */
    if (!GetSecurityDescriptorControl(sd, &sdc, &sdc_rev)) {
      LocalFree(sd);
      unsn_acl_fail("Error getting ACL control information", GetLastError());
    }

    if (!(sdc & SE_DACL_PROTECTED)) { /* No control flags we care about */
      LocalFree(sd);

#ifdef ACL_DEBUG
      printf_s(" ---> Empty ACL (no explicit ACE, may have inherited ACE)\n");
#endif
      CAMLreturn(UNSN_ACL_EMPTY); /* Empty ACL (or only inherited) */
    }
  }

  res2 = ConvertSecurityDescriptorToStringSecurityDescriptor(sd,
           SDDL_REVISION_1, si, &acl_text, NULL);
  err = GetLastError();

  LocalFree(sd);

  if (!res2 || (acl_text == NULL)) {
    unsn_acl_fail("Error converting ACL to text", err);
  }

#ifdef ACL_DEBUG
  printf_s(" ---> Final ACL text representation:   %ls\n", acl_text);
#endif

  result = caml_copy_string_of_utf16(acl_text);

  LocalFree(acl_text);

  CAMLreturn(result);
}


#else /* defined(_WIN32) */


#if defined(__Solaris__) || defined(__APPLE__)
#include <fcntl.h>
#include <sys/stat.h>
#endif

#if defined(__Solaris__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/acl.h>
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <unistd.h>
#endif

#if defined(__APPLE__)
#define UNSN_ACL_T acl_t
#else
#define UNSN_ACL_T acl_t *
#endif


static void unsn_acl_fail(const char *fmtmsg)
{
  char errmsg[255];

  int errnum = errno;
  if (strerror_r(errnum, errmsg, sizeof(errmsg)) != 0) {
    snprintf(errmsg, sizeof(errmsg), "(error code %d)", errnum);
  }

  caml_failwith_value(caml_alloc_sprintf(fmtmsg, errmsg));
}

#if defined(__FreeBSD__) || defined(__NetBSD__)
static acl_type_t unsn_path_acl_type(const char *path)
{
  if (pathconf(path, _PC_ACL_NFS4) > 0) { /* NFSv4 ACL supported */
    return ACL_TYPE_NFS4;
  } else if (pathconf(path, _PC_ACL_EXTENDED) > 0) { /* POSIX draft ACL */
    return ACL_TYPE_ACCESS; /* It is not possible to get or set
                               default and access ACL at the same time,
                               so fall back to access ACL only. */
  } else { /* ACLs not supported */
    return -1;
  }
}
#elif defined(__APPLE__)
static acl_type_t unsn_path_acl_type(const char *path)
{
  return ACL_TYPE_EXTENDED;
}
#endif


static void unsn_remove_acl_os(const char *path)
{
#if defined(__Solaris__)
  struct stat st;

  if (stat(path, &st) != 0) {
    unsn_acl_fail("Can't access file to remove ACL: %s");
  }

  if (acl_strip(path, st.st_uid, st.st_gid, st.st_mode) != 0) {
    unsn_acl_fail("Error removing ACL: %s");
  }
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  /* FreeBSD has a acl_strip_np() function, but it would be
   * much too complicated in this code. */
  /* Don't even bother checking for target ACL type, just
   * try to remove all and ignore errors. */
  acl_delete_file_np(path, ACL_TYPE_DEFAULT);
  acl_delete_file_np(path, ACL_TYPE_ACCESS);
  acl_delete_file_np(path, ACL_TYPE_NFS4);
#elif defined(__APPLE__)
  acl_set_file(path, unsn_path_acl_type(path), acl_from_text("!#acl 1"));
#endif
}


/************************************
 *         Set ACL from text
 ************************************/
static _Bool unsn_acl_from_text_os(const char *acl_text, UNSN_ACL_T *aclp)
{
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  *aclp = acl_from_text(acl_text);

  return (*aclp != NULL);
#elif defined(__Solaris__)
  int error = acl_fromtext(acl_text, aclp);

  return (error == 0 && aclp != NULL);
#endif
}

CAMLprim value unison_acl_from_text(value path, value acl)
{
  CAMLparam2(path, acl);
  const char *acl_text = String_val(acl);
  const char *name = String_val(path);
  UNSN_ACL_T aclp = NULL;
  int error;

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  acl_type_t type = unsn_path_acl_type(name);
  if (type == -1) {
    caml_failwith("ACL not supported on this path");
  }
#endif

  /* Check if ACL must be removed */
  if (*acl_text == '\0') {
    unsn_remove_acl_os(name);
    CAMLreturn(Val_unit);
  }

  if (!unsn_acl_from_text_os(acl_text, &aclp)) {
    caml_failwith("Error converting ACL from text");
  }

#if defined(__Solaris__)
  error = acl_set(name, aclp);
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  error = acl_set_file(name, type, aclp);
#endif
  int real_err = errno;
  acl_free(aclp);
  errno = real_err;

  if (error == -1) {
    unsn_acl_fail("Error setting ACL: %s");
  }

  CAMLreturn(Val_unit);
}


/************************************
 *          Get ACL as text
 ************************************/
/* This function does not allocate new,
 * it returns the pointer to its argument. */
static char *postprocess_acl_os(char *s)
{
#if defined(__FreeBSD__) || defined(__NetBSD__)
  char *p;
  char *buf = s;  /* Just an alias; modify input string in place */
  int perms = 0, comment = 0, offs = 0;

  for (p = s; *p; p++) {
    switch (*p) {
      case ',' :
          perms = 0;
          break;
      case '#' :
          /* FreeBSD acl_to_text embeds the #effective permissions,
           * which are actually not part of the ACL. */
          comment = 1;
          break;
      case '@' :
      case ':' :
          if (!comment) {
            perms++;
          }
          break;
      case 'D' :
          /* Swap the position of d and D permissions.
           * Synchronization works even without swapping, but the different
           * ordering will show up as constant synchronization difference. */
          if (perms == 2) {
            if (buf[offs - 1] != 'd' && *(p + 1) == 'd') {
              *p = 'd';
              *(p + 1) = 'D';
            } else if (buf[offs - 1] != 'd' && *(p + 1) == '-') {
              *p = '-';
              *(p + 1) = 'D';
            }
            perms = 0; /* prevent further swapping */
          }
          break;
      case 'd' :
          if (perms == 2) {
            if (buf[offs - 1] == '-' && *(p + 1) != 'D' && *(p + 1) != '\0') {
              buf[offs - 1] = 'd';
              *p = '-';
            } else if (buf[offs - 1] != 'd' && *(p + 1) == '-') {
              *p = '-';
              *(p + 1) = 'D';
            }
            perms = 0; /* prevent further swapping */
          }
          break;
      case '\n' :
          /* Replace newlines with commas...
           * ... except if it's the last one. */
          if (*(p + 1) != '\0') {
            *p = ',';
          } else {
            *p = ' ';
          }
          perms = 0;
          comment = 0;
          break;
    }

    /* Remove all whitespace and comments. */
    if (*p != ' ' && *p != '\t' && !comment) {
      buf[offs++] = *p;
    }
  }
  buf[offs] = '\0';

  return buf;
#elif defined(__APPLE__)
  /* Remove trailing newline */
  size_t last = strlen(s) - 1;
  if (last >= 0 && s[last] == '\n') {
    s[last] = '\0';
  }

  return s;
#endif
}

static char *unsn_acl_to_text_os(UNSN_ACL_T aclp)
{
#if defined(__FreeBSD__) || defined(__NetBSD__)
  return postprocess_acl_os(acl_to_text_np(aclp, NULL, ACL_TEXT_APPEND_ID));
#elif defined(__APPLE__)
  return postprocess_acl_os(acl_to_text(aclp, NULL));
#elif defined(__Solaris__)
  return acl_totext(aclp, ACL_APPEND_ID | ACL_COMPACT_FMT | ACL_SID_FMT);
#endif
}

static _Bool unsn_acl_is_empty_or_trivial_os(UNSN_ACL_T aclp)
{
#if defined(__FreeBSD__) || defined(__NetBSD__)
  int is_trivial = 0;

  acl_is_trivial_np(aclp, &is_trivial); /* Ignore any errors here */

  return (is_trivial || aclp == NULL);
#elif defined(__APPLE__) || defined(__Solaris__)
  return (aclp == NULL);
#endif
}

static int unsn_get_acl_os(const char *path, UNSN_ACL_T *aclp)
{
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  acl_type_t type = unsn_path_acl_type(path);
  if (type == -1) {
    errno = EOPNOTSUPP;
    return -1;
  }

  errno = 0;
  *aclp = acl_get_file(path, type);

#if defined(__APPLE__)
  if (errno != 0) {
    /* ACLs are always enabled on Darwin since version 10 (2009).
     * Unfortunately, Darwin sets errno to ENOENT also when the file
     * does not have an extended ACL. Since it is impossible to distinguish
     * from the real ENOENT (due to the path), we must check with stat(). */
    if (errno == ENOENT) {
      struct stat st;
      errno = 0;
      stat(path, &st);

      if (errno != ENOENT) {
        /* The path does not trigger ENOENT;
         * this means that there is no extended ACL. This is allowed. */
        return 0;
      }

      errno = ENOENT; /* Ignore errno from stat(), restore original errno. */
    }

    return -1;
  }
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  if (*aclp == NULL) {
    return -1;
  }
#endif

  return 0;
#endif  /* FreeBSD or NetBSD or Darwin */

#if defined(__Solaris__)
  return acl_get(path, ACL_NO_TRIVIAL, aclp);
#endif
}

CAMLprim value unison_acl_to_text(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  UNSN_ACL_T aclp = NULL;
  char *acltxt;

  int err = unsn_get_acl_os(String_val(path), &aclp);
  if (err == -1) {
    if (errno == ENOSYS || errno == EOPNOTSUPP) {
      CAMLreturn(UNSN_ACL_NOT_SUPPORTED);
    } else {
      unsn_acl_fail("Error getting ACL: %s");
    }
  }

  /* If there was no error but aclp is NULL then it means an empty
   * or trivial ACL (that is, just the mode), which is allowed. */
  if (aclp == NULL || unsn_acl_is_empty_or_trivial_os(aclp)) {
    if (aclp != NULL) {
      acl_free(aclp);
    }

    CAMLreturn(UNSN_ACL_EMPTY);
  }

  acltxt = unsn_acl_to_text_os(aclp);
  acl_free(aclp);

  if (acltxt == NULL) {
    caml_failwith("Error converting ACL to text");
  }

  result = caml_copy_string(acltxt);
  free(acltxt);

  CAMLreturn(result);
}

#endif /* !defined(_WIN32) */


#endif /* UNSN_HAS_FS_ACL */
