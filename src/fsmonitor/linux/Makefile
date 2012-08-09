
FSMONITOR = $(NAME)-fsmonitor

DIR=fsmonitor/linux
FSMCAMLOBJS = \
   lwt/lwt.cmx lwt/pqueue.cmx lwt/generic/lwt_unix_impl.cmx lwt/lwt_unix.cmx \
   $(DIR)/inotify.cmx $(DIR)/lwt_inotify.cmx \
   fsmonitor/watchercommon.cmx $(DIR)/watcher.cmx
FSMCOBJS = \
  $(DIR)/inotify_stubs.o
FSMCAMLLIBS=unix.cmxa

buildexecutable:: $(FSMONITOR)$(EXEC_EXT)

$(FSMONITOR)$(EXEC_EXT): $(FSMCAMLOBJS) $(FSMCOBJS)
	@echo Linking $@
	$(OCAMLOPT) -verbose $(CAMLFLAGS) -o $@ $(CFLAGS) $(FSMCAMLLIBS) $^ $(CLIBS)

clean::
	rm -f $(DIR)/*.cm[iox] $(DIR)/*.o $(DIR)/*~
	rm -f $(FSMONITOR)$(EXEC_EXT)