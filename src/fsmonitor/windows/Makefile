
FSMONITOR = $(NAME)-fsmonitor

DIR=fsmonitor/windows
FSMCAMLOBJS = \
   ubase/rx.cmx unicode_tables.cmx unicode.cmx \
   system/system_generic.cmx system/system_win.cmx \
   system/win/system_impl.cmx \
   lwt/lwt.cmx lwt/pqueue.cmx lwt/win/lwt_unix_impl.cmx lwt/lwt_unix.cmx \
   lwt/win/lwt_win.cmx \
   $(DIR)/shortnames.cmx fsmonitor/watchercommon.cmx $(DIR)/watcher.cmx
FSMCOBJS = \
   system/system_win_stubs.o lwt/lwt_unix_stubs.o $(DIR)/shortnames_stubs.o
FSMCAMLLIBS=bigarray.cmxa unix.cmxa

buildexecutable:: $(FSMONITOR)$(EXEC_EXT)

$(FSMONITOR)$(EXEC_EXT): $(FSMCAMLOBJS) $(FSMCOBJS)
	@echo Linking $@
	$(OCAMLOPT) -verbose $(CAMLFLAGS) -o $@ $(CFLAGS) $(FSMCAMLLIBS) $^ $(CLIBS)

clean::
	rm -f $(DIR)/*.cm[iox] $(DIR)/*.o $(DIR)/*~
	rm -f $(FSMONITOR)$(EXEC_EXT)
