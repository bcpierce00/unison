/* Unison file synchronizer: src/props_xattr.c */
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

/* Conceptually, here, an extended attribute is just a name-value pair,
 * where name is a text string and value is a binary string. This matches
 * well the concept of extended attributes on some platforms (notably,
 * Linux and BSDs), while some platforms provide more sophisticated
 * extended attributes.
 *
 * The external interface is defined as follows. Every supported platform
 * must implement this interface. xattr format can be platform-specific,
 * which may prevent cross-platform synchronization but still allows
 * synchronization within the platform. Cross-platform synchronization may
 * still be possible in some cases, even if one platform will not
 * understand the xattrs; the attribute values are treated as blobs then.
 *
 *
 * SET the value of one xattr
 * ==========================
 * unit unison_xattr_set(String path, String xattrname, String xattrvalue)
 *
 *   Create the requested extended attribute on the requested file or
 *   directory and set the attribute value.
 *   If the attribute already exists then its value is overwritten.
 *   Symbolic links are followed.
 *
 * Input parameters
 *   path       - absolute path of a file or directory
 *   xattrname  - name of attribute to set on the path
 *   xattrvalue - value of attribute to set on the path
 *
 * Return value
 *   No return value.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MAY be raised when extended attributes are not supported on
 *   the requested path.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to set the attribute
 *     Error creating the attribute (invalid name, permission error, etc.)
 *     Error setting the attribute value
 *
 *
 * REMOVE one xattr
 * ================
 * unit unison_xattr_remove(String path, String xattrname)
 *
 *   Remove the requested extended attribute on the requested file or
 *   directory. Symbolic links are followed.
 *
 * Input parameters
 *   path      - absolute path of a file or directory
 *   xattrname - name of attribute to remove on the path
 *
 * Return value
 *   No return value.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MAY be raised when extended attributes are not supported on
 *   the requested path.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to remove the attribute
 *     Error removing the attribute
 *
 *
 * GET the value of one xattr
 * ==========================
 * String unison_xattr_get(String path, String xattrname)
 *
 *   Get the value of the requested extended attribute on the requested
 *   file or directory. The entire value is returned in full length.
 *   Symbolic links are followed.
 *
 * Input parameters
 *   path      - absolute path of a file or directory
 *   xattrname - name of attribute to get on the path
 *
 * Return value
 *   The value of the requested extended attribute, as a binary string.
 *
 * Exceptions
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MAY be raised when extended attributes are not supported on
 *   the requested path.
 *   Failure MUST be raised when:
 *     The attribute value does not fit in an OCaml string on a 32-bit
 *     platform (approx. 16 MB)
 *     Can't access file to read the attribute
 *     Error reading the attribute value or attribute not found
 *
 *
 * GET the list of xattrs with value lengths
 * =========================================
 * List of (String * Int) unison_xattrs_list(String path)
 *
 *   Get the list of all extended attributes on the requested file or
 *   directory. Attributes names are returned together with the length
 *   of attribute values.
 *   Attributes in the list can be returned in any order and the order
 *   does not have to be stable (i.e. it can be different on every
 *   invocation on the same path).
 *   Symbolic links are followed.
 *
 * Input parameters
 *   path - absolute path of a file or directory
 *
 * Return value
 *   The list of name-length pairs, with each pair representing the
 *   name and length of value of one extended attribute.
 *
 * Exceptions
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MUST be raised when extended attributes are not supported on
 *   the requested path, or should not otherwise be returned.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to get the attributes
 *     Error reading attribute values
 *
 *
 * Indicate platform/system capabilities
 * =====================================
 * Boolean unison_xattr_updates_ctime()
 *
 *   Indicate whether the platform/system updates file ctime when
 *   extended attributes change on the file. Not all platforms do this
 *   (Solaris/illumos are known to not update any stat times of the
 *   file/directory when its extended attributes are modified).
 *   The capabilities of the system are important to know because
 *   detecting updates quickly yet correctly relies on knowing when to
 *   get the list of xattrs with values.
 *
 * Input parameters
 *   none
 *
 * Return value
 *   True if file ctime is updated at xattr changes. False otherwise.
 *   For a platform that does not support ctime, true can be returned
 *   if xattr changes update file mtime.
 *
 * Exceptions
 *   none
 *
 */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>


#if defined(sun) || defined(__sun)  /* Solarish, all illumos-based OS,   */
#define __Solaris__                 /* OpenIndiana, OmniOS, SmartOS, ... */
#endif

#undef UNSN_HAS_XATTR
#if defined(__Solaris__) || defined(__FreeBSD__) || defined(__NetBSD__) \
    || defined(__APPLE__) || defined(__linux)
#define UNSN_HAS_XATTR
#endif

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif

#define UNSN_XATTR_NOT_SUPPORTED_EX "XattrNotSupported"


static void unsn_xattr_not_supported()
{
  static const value *ex = NULL;

  if (ex == NULL) {
    ex = caml_named_value(UNSN_XATTR_NOT_SUPPORTED_EX);
  }

  caml_raise_constant(*ex);
}


#ifndef UNSN_HAS_XATTR

CAMLprim void unison_xattr_set(value path, value xattrname, value xattr)
{
  unsn_xattr_not_supported();
}

CAMLprim void unison_xattr_remove(value path, value xattrname)
{
  unsn_xattr_not_supported();
}

CAMLprim void unison_xattr_get(value path, value xattrname)
{
  unsn_xattr_not_supported();
}

CAMLprim void unison_xattrs_list(value path)
{
  unsn_xattr_not_supported();
}

CAMLprim value unison_xattr_updates_ctime(value unit)
{
  CAMLparam0();
  CAMLreturn(Val_true);
}

#else /* UNSN_HAS_XATTR */


#if defined(__Solaris__)
#ifndef _ATFILE_SOURCE
#define _ATFILE_SOURCE 1
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__)
#include <errno.h>
#include <sys/types.h>
#include <sys/extattr.h>
#include <string.h>
#include <stdio.h>
#endif

#if defined(__FreeBSD__)
#define ENOTSUP EOPNOTSUPP
#endif

#if defined(__APPLE__)
#include <errno.h>
#include <sys/xattr.h>
#include <string.h>
#include <stdio.h>
#endif

#if defined(__linux)
#include <errno.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <string.h>
#include <stdio.h>

/* Attribute names on Linux must be mangled to make cross-platform
 * synchronization possible. When listing attributes, the "user."
 * prefix is removed for user namespace attributes and an "!" is
 * prepended to attribute names in all other namespaces (or more
 * accurately, it is prepended to the namespace name).
 *
 * When feeding the attribute names to get, set, remove and other
 * syscalls, the reverse is done. */

#define XN_BUF_LEN 261
#define XN_LEN (XN_BUF_LEN - 6)

static value val_of_attrname(char *attrname)
{
  char buf[XN_BUF_LEN] = "!";

  if (attrname != NULL) {
    if (strncmp(attrname, "user.", 5) == 0) {
      attrname += 5;
    } else {
      attrname = strncat(buf, attrname, XN_LEN);
    }
  }

  return caml_copy_string(attrname != NULL ? attrname : "");
}

static const char *attrname_of_val(const char *attrname, char *buf)
{
  if (attrname != NULL) {
    if (attrname[0] == '!') {
      return attrname + 1;
    } else {
      return strncat(strcpy(buf, "user."), attrname, XN_LEN);
    }
  } else {
    return attrname;
  }
}
#endif /* defined(__linux) */


#if defined(__linux)

#define XATTRNAME_VAL(a, n) char xnb_[XN_BUF_LEN];\
                            const char *a = attrname_of_val(String_val(n), xnb_)
#define VAL_XATTRNAME val_of_attrname

#else

#define XATTRNAME_VAL(a, n) const char *a = String_val(n)
#define VAL_XATTRNAME caml_copy_string

#endif /* defined(__linux) */


static void unsn_xattr_fail(const char *fmtmsg)
{
  char buf[512];
  char *errmsg;

#if defined(_WIN32)
  errmsg = strerror(errno);
#else
  int errnum = errno;
  if (strerror_r(errnum, buf, sizeof(buf)) != 0) {
    snprintf(buf, sizeof(buf), "(error code %d)", errnum);
  }
  errmsg = buf;
#endif /* defined(_WIN32) */

  caml_failwith_value(caml_alloc_sprintf(fmtmsg, errmsg));
}

static int unsn_is_system_attr_os(const char *attrname)
{
#if defined(__linux)
  return (strncmp(attrname, "system.", 7) == 0 &&
          strncmp(attrname, "system.posix_acl_", 17) != 0);
#elif defined(__APPLE__)
  return (strcmp(attrname, XATTR_FINDERINFO_NAME) == 0 ||
          strcmp(attrname, XATTR_RESOURCEFORK_NAME) == 0);
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  return 0;
#elif defined(__Solaris__)
  /* Special system "extensible attributes" xattrs are defined in sys/attr.h
   * as VIEW_READONLY = "SUNWattr_ro" and VIEW_READWRITE = "SUNWattr_rw" */
  return (strcmp(attrname, ".") == 0 || strcmp(attrname, "..") == 0 ||
          strncmp(attrname, "SUNWattr_", 9) == 0);
#endif
}


/************************************
 *            Set xattr
 ************************************/
static int unsn_set_xattr_os(const char *path, const char *attrname,
                             const void *attrvalue, size_t valuesize)
{
#if defined(__linux)
  return setxattr(path, attrname, attrvalue, valuesize, 0);
#elif defined(__APPLE__)
  return setxattr(path, attrname, attrvalue, valuesize, 0, 0);
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  return (int) extattr_set_file(path, EXTATTR_NAMESPACE_USER, attrname,
                                attrvalue, valuesize);
#elif defined(__Solaris__)
  if (pathconf(path, _PC_XATTR_ENABLED) < 1) {
    unsn_xattr_not_supported();
  }

  /* This is a simplified implementation that just creates/opens
   * the xattr and writes the value into it.
   *
   * Extended attributes in Solaris and illumos are much more
   * flexible. In most ways they are like normal files/directories.
   * They have owner/group, mode, utimes, even ACL, and can have
   * their own extended attributes, etc.
   *
   * This implementation does not synchronize any of those params,
   * as xattrs are conceptually treated as name-value pairs.
   * It is unknown if this will cause problems with real use cases. */
  int fd = attropen(path, attrname, O_CREAT|O_WRONLY|O_TRUNC|O_CLOEXEC);
  if (fd == -1) {
    unsn_xattr_fail("Error opening extended attribute for writing: %s");
  }

  ssize_t written = 0, c;
  do {
    c = write(fd, attrvalue + written, valuesize - written);
    written += c;
  } while (c > 0 && written < valuesize);

  close(fd);

  return c == -1 ? c : 0;
#endif
}

CAMLprim value unison_xattr_set(value path, value xattrname, value xattr)
{
  CAMLparam3(path, xattrname, xattr);
  const char *name = String_val(path);
  XATTRNAME_VAL(attr, xattrname);
  const char *attrvalue = String_val(xattr);
  unsigned int len;

  /* Ignore system extended attributes */
  if (unsn_is_system_attr_os(attr)) {
    CAMLreturn(Val_unit);
  }

  len = caml_string_length(xattr);

  int error = unsn_set_xattr_os(name, attr, attrvalue, len);
  if (error == -1) {
    if (errno == ENOTSUP) {
      unsn_xattr_not_supported();
    } else {
      unsn_xattr_fail("Error writing extended attribute: %s");
    }
  }

  CAMLreturn(Val_unit);
}


/************************************
 *           Remove xattr
 ************************************/
static int unsn_remove_xattr_os(const char *path, const char *attrname)
{
#if defined(__linux)
  return removexattr(path, attrname);
#elif defined(__APPLE__)
  return removexattr(path, attrname, 0);
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  return (int) extattr_delete_file(path, EXTATTR_NAMESPACE_USER, attrname);
#elif defined(__Solaris__)
  if (pathconf(path, _PC_XATTR_ENABLED) < 1) {
    unsn_xattr_not_supported();
  }

  int fd = attropen(path, ".", O_RDONLY|O_CLOEXEC);
  if (fd == -1) {
    unsn_xattr_fail("Error opening extended attribute for removing: %s");
  }
  int error = unlinkat(fd, attrname, 0);
  close(fd);

  return error;
#endif
}

CAMLprim value unison_xattr_remove(value path, value xattrname)
{
  CAMLparam2(path, xattrname);
  const char *name = String_val(path);
  XATTRNAME_VAL(attr, xattrname);

  /* Ignore system extended attributes */
  if (unsn_is_system_attr_os(attr)) {
    CAMLreturn(Val_unit);
  }

  int error = unsn_remove_xattr_os(name, attr);
  if (error == -1 && errno == ENOTSUP) {
    unsn_xattr_not_supported();
  }

  CAMLreturn(Val_unit);
}


/************************************
 *         Length of xattr
 ************************************/
static ssize_t unsn_length_xattr_os(const char *path, const char *attrname)
{
#if defined(__linux)
  return getxattr(path, attrname, NULL, 0);
#elif defined(__APPLE__)
  return getxattr(path, attrname, NULL, 0, 0, 0);
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  return extattr_get_file(path, EXTATTR_NAMESPACE_USER, attrname, NULL, 0);
#elif defined(__Solaris__)
  int fd = attropen(path, attrname, O_RDONLY|O_CLOEXEC);
  if (fd == -1) {
    unsn_xattr_fail("Error opening extended attribute for querying length: %s");
  }

  struct stat buf;
  int error;

  error = fstat(fd, &buf);
  close(fd);

  return error == -1 ? error : buf.st_size;
#endif
}


/************************************
 *            Get xattrs
 ************************************/
static ssize_t unsn_get_xattr_os(const char *path, const char *attrname,
                                 void *buf, size_t size)
{
#if defined(__linux)
  return getxattr(path, attrname, buf, size);
#elif defined(__APPLE__)
  return getxattr(path, attrname, buf, size, 0, 0);
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  return extattr_get_file(path, EXTATTR_NAMESPACE_USER, attrname, buf, size);
#elif defined(__Solaris__)
  int fd = attropen(path, attrname, O_RDONLY|O_CLOEXEC);
  if (fd == -1) {
    unsn_xattr_fail("Error opening extended attribute for reading: %s");
  }

  ssize_t rd = 0, c;
  do {
    c = read(fd, buf + rd, size - rd);
    rd += c;
  } while (c > 0 && rd < size);

  close(fd);

  return c == -1 ? c : rd;
#endif
}

CAMLprim value unison_xattr_get(value path, value xattrname)
{
  CAMLparam2(path, xattrname);
  CAMLlocal1(v);
  const char *name = String_val(path);
  XATTRNAME_VAL(attr, xattrname);

  int len = 0, tries = 0;

  do {
    if (++tries > 10) {
      caml_failwith("Error reading contents of extended attribute; "
        "it keeps changing");
    }

    len = unsn_length_xattr_os(name, attr);
    if (len == -1 && errno == ENOTSUP) {
      unsn_xattr_not_supported();
    } else if (len == -1) {
      unsn_xattr_fail("Error reading length of extended attribute: %s");
    }

    if (len == 0) {
      v = caml_alloc_string(0);
    } else if (len > 16777211) { // Max OCaml string length on 32 bit platforms
      caml_failwith_value(caml_alloc_sprintf(
        "Extended attribute value is too big (%d bytes)", len));
    } else {
      char *buf = malloc(len);

      len = unsn_get_xattr_os(name, attr, buf, len);
      if (len == -1 && errno == ENOTSUP) {
        free(buf);
        unsn_xattr_not_supported();
      } else if (len == -1 && errno != ERANGE) {
        free(buf);
        unsn_xattr_fail("Error reading contents of extended attribute: %s");
      } else if (len != -1) {
        v = caml_alloc_initialized_string(len, buf);
      }

      free(buf);
    }
  } while (len == -1 && errno == ERANGE);
  /* ERANGE error produced on Linux and Darwin; other platforms
   * truncate the value if buffer is too small. */

  CAMLreturn(v);
}


/************************************
 *           List xattrs
 ************************************/
static void unsn_list_xattr_fail(void)
{
  unsn_xattr_fail("Error getting list of extended attributes: %s");
}

#if !defined(__Solaris__)

static ssize_t unsn_list_xattr_os(const char *path, char *buf, size_t size)
{
#if defined(__linux)
  return listxattr(path, buf, size);
#elif defined(__APPLE__)
  return listxattr(path, buf, size, 0);
#elif defined(__FreeBSD__) || defined(__NetBSD__)
  return extattr_list_file(path, EXTATTR_NAMESPACE_USER, buf, size);
#endif
}

static ssize_t unsn_list_xattr_aux(const char *path, char **buf)
{
  ssize_t namelen;

  namelen = unsn_list_xattr_os(path, NULL, 0);

  if (namelen == -1) {
    if (errno == ENOTSUP) {
      unsn_xattr_not_supported();
    }
    unsn_list_xattr_fail();
  }
  if (namelen == 0) {
    return 0;
  }

  *buf = malloc(namelen);
  if (*buf == NULL) {
    unsn_list_xattr_fail();
  }

  namelen = unsn_list_xattr_os(path, *buf, namelen);
  if (namelen == -1) {
    free(*buf);
    unsn_list_xattr_fail();
  }
  if (namelen == 0) {
    free(*buf);
    return 0;
  }

  return namelen;
}

#else

static ssize_t unsn_list_xattr_aux(const char *path, DIR **dirp)
{
  if (pathconf(path, _PC_XATTR_ENABLED) < 1) {
    unsn_xattr_not_supported();
  }

  if (pathconf(path, _PC_XATTR_EXISTS) < 1) {
    return 0;
  }

  int fd = attropen(path, ".", O_RDONLY|O_CLOEXEC);
  if (fd == -1) {
    unsn_list_xattr_fail();
  }

  *dirp = fdopendir(fd);
  if (*dirp == NULL) {
    int real_err = errno;
    close(fd);
    errno = real_err;
    unsn_list_xattr_fail();
  }

  return 1;
}

#endif /* !__Solaris__ */

CAMLprim value unison_xattrs_list(value path)
{
  CAMLparam1(path);
  CAMLlocal3(result, p, l);
  /* Use a static buffer because memory management for the path would become
   * much too complex. Use a constant length because PATH_MAX is not reliable
   * and pathconf() is out of question. */
  char name[32768];
#if !defined(__Solaris__)
  char *xattrs;
#else
  DIR *xattrs;
  struct dirent *dp;
  char *xattrname;
#endif
  ssize_t namelen, len;

  if (caml_string_length(path) > 32767) {
    caml_failwith("The path is too long");
  }
  strcpy(name, String_val(path));

  result = Val_emptylist;

  namelen = unsn_list_xattr_aux(name, &xattrs);
  if (namelen == 0) {
    CAMLreturn(result);
  }

#if defined(__FreeBSD__) || defined(__NetBSD__)
  size_t nl = 0;
  char xattrname[256];

  for (char *xattrnamep = xattrs; xattrnamep < xattrs + namelen;
                                  xattrnamep += nl + 1) {
    nl = *xattrnamep & 255;
    memcpy(xattrname, xattrnamep + 1, nl);
    xattrname[nl] = '\0';
#elif !defined(__Solaris__)
  /* For safety */
  *(xattrs + namelen - 1) = '\0';

  for (char *xattrname = xattrs; xattrname < xattrs + namelen;
                                 xattrname += strlen(xattrname) + 1) {
#elif defined(__Solaris__)
  while (dp = readdir(xattrs)) {
    /* Note: NULL is returned for both end of dir and an error condition.
     * Error conditions are silently ignored. */
    xattrname = dp->d_name;
#endif

    /* Ignore system extended attributes */
    if (unsn_is_system_attr_os(xattrname)) {
      continue;
    }

    len = unsn_length_xattr_os(name, xattrname);
    if (len == -1) {
      continue; /* Ignore silently */
    }

    p = caml_alloc_tuple(2);
    Store_field(p, 0, VAL_XATTRNAME(xattrname));
    Store_field(p, 1, Val_int(len));

    l = caml_alloc_small(2, Tag_cons);
    Field(l, 0) = p;
    Field(l, 1) = result;

    result = l;
  }

#if defined(__Solaris__)
  closedir(xattrs);
#else
  free(xattrs);
#endif

  CAMLreturn(result);
}


/************************************
 *        ctime capabilities
 ************************************/
CAMLprim value unison_xattr_updates_ctime(value unit)
{
  CAMLparam0();
#if defined(__Solaris__)
  CAMLreturn(Val_false);
#else
  CAMLreturn(Val_true);
#endif
}


#endif /* UNSN_HAS_XATTR */
