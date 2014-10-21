
FSMONITOR = $(NAME)-fsmonitor

DIR=fsmonitor/linux
FSMOCAMLOBJS = \
   lwt/lwt.cmo lwt/pqueue.cmo lwt/generic/lwt_unix_impl.cmo lwt/lwt_unix.cmo \
   $(DIR)/inotify.cmo $(DIR)/lwt_inotify.cmo \
   fsmonitor/watchercommon.cmo $(DIR)/watcher.cmo
FSMCOBJS = \
  $(DIR)/inotify_stubs.o
FSMOCAMLLIBS=unix.cma

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