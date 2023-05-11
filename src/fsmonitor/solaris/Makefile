
FSMONITOR = $(NAME)-fsmonitor

DIR=fsmonitor/solaris
FSMOCAMLOBJS = \
   lwt/lwt.cmo lwt/pqueue.cmo lwt/generic/lwt_unix_impl.cmo lwt/lwt_unix.cmo \
   fsmonitor/watchercommon.cmo ubase/safelist.cmo $(DIR)/watcher.cmo

FSMCOBJS = $(DIR)/fen_stubs.o
FSMOCAMLLIBS=unix.cma

ifeq ($(NATIVE), true)
  FSMCAMLOBJS=$(subst .cmo,.cmx, $(FSMOCAMLOBJS))
  FSMCAMLLIBS=$(subst .cma,.cmxa, $(FSMOCAMLLIBS))
else
  FSMCAMLOBJS=$(FSMOCAMLOBJS)
  FSMCAMLLIBS=$(FSMOCAMLLIBS)
endif

buildexecutable: $(FSMONITOR)$(EXEC_EXT)

$(FSMONITOR)$(EXEC_EXT): $(FSMCAMLOBJS) $(FSMCOBJS)
	@echo Linking $@
	$(CAMLC) -verbose $(CAMLFLAGS) $(CAMLLDFLAGS) -o $@ $(CAMLCFLAGS) $(FSMCAMLLIBS) $^ $(CLIBS)

clean::
	rm -f $(DIR)/*.cm[iox] $(DIR)/*.o $(DIR)/*~
	rm -f $(FSMONITOR)$(EXEC_EXT)
