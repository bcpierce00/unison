
FSMONITOR = $(NAME)-fsmonitor

DIR=fsmonitor/windows
FSMOCAMLOBJS = \
   ubase/rx.cmo unicode_tables.cmo unicode.cmo \
   system/system_generic.cmo system/system_win.cmo \
   system/win/system_impl.cmo \
   lwt/lwt.cmo lwt/pqueue.cmo lwt/win/lwt_unix_impl.cmo lwt/lwt_unix.cmo \
   lwt/win/lwt_win.cmo \
   $(DIR)/shortnames.cmo fsmonitor/watchercommon.cmo $(DIR)/watcher.cmo
FSMCOBJS = \
   system/system_win_stubs.o lwt/lwt_unix_stubs.o $(DIR)/shortnames_stubs.o
FSMOCAMLLIBS=bigarray.cma unix.cma

ifeq ($(NATIVE), true)
  FSMCAMLOBJS=$(subst .cmo,.cmx, $(FSMOCAMLOBJS))
  FSMCAMLLIBS=$(subst .cma,.cmxa, $(FSMOCAMLLIBS))
else
  FSMCAMLOBJS=$(FSMOCAMLOBJS)
  FSMCAMLLIBS=$(FSMOCAMLLIBS)
endif

buildexecutable:: $(FSMONITOR)$(EXEC_EXT)

$(FSMONITOR)$(EXEC_EXT): $(FSMCAMLOBJS) $(FSMCOBJS)
	@echo Linking $@
	$(CAMLC) -verbose $(CAMLFLAGS) -o $@ $(CFLAGS) $(FSMCAMLLIBS) $^ $(CLIBS)

clean::
	rm -f $(DIR)/*.cm[iox] $(DIR)/*.o $(DIR)/*~
	rm -f $(FSMONITOR)$(EXEC_EXT)
