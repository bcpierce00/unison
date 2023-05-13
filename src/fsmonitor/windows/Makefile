FSMOCAMLOBJS = \
   ubase/umarshal.cmo \
   ubase/rx.cmo unicode_tables.cmo unicode.cmo \
   system/system_generic.cmo system/system_win.cmo \
   system/win/system_impl.cmo \
   lwt/lwt.cmo lwt/pqueue.cmo lwt/win/lwt_unix_impl.cmo lwt/lwt_unix.cmo \
   lwt/win/lwt_win.cmo \
   fsmonitor/watchercommon.cmo $(FSMDIR)/watcher.cmo
FSMCOBJS = \
   bytearray_stubs$(OBJ_EXT) \
   system/system_win_stubs$(OBJ_EXT) lwt/lwt_unix_stubs$(OBJ_EXT) \
   props_xattr$(OBJ_EXT) props_acl$(OBJ_EXT)
FSMOCAMLLIBS=unix.cma

# Additional dependencies
$(FSMDIR)/watcher.cmo: lwt/win/lwt_win.cmo
$(FSMDIR)/watcher.cmx: lwt/win/lwt_win.cmx
