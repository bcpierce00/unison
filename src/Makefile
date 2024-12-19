# Unison file synchronizer: src/Makefile
# Copyright 1999-2024 (see ../LICENSE for terms).

# IMPORTANT!
#
# This file is portable and compatible with GNU Make, BSD make, Solaris
# (d)make and NMAKE. Do not make any changes in this file unless you are
# certain that the changes do not break this portability.

########################################################################
########################################################################
#     (There should be no need to change anything from here on)       ##
########################################################################

default: all
.PHONY: default

# Executed only by BSD make
.BEGIN:
	@touch _mk.cfg
	@ocaml make_tools.ml conf > Makefile.cfg
	@ocaml make_tools.ml conf2 > Makefile2.cfg

# Executed only by Solaris dmake
.INIT:
	@touch _mk.cfg
	@ocaml make_tools.ml conf ASSUME_DMAKE=true > Makefile.cfg
	@ocaml make_tools.ml conf2 ASSUME_DMAKE=true > Makefile2.cfg

# Executed only by GNU Make and NMAKE
Makefile.cfg: _mk.cfg
	@ocaml make_tools.ml conf > Makefile.cfg
	@ocaml make_tools.ml conf2 MAKE="$(MAKE)" > Makefile2.cfg
_mk.cfg: ;

TARGETS = all tui gui macui fsmonitor manpage docs clean depend dependgraph paths
FILES = unison unison.exe

.NOTPARALLEL:
.PHONY: $(TARGETS)
$(TARGETS) $(FILES): Makefile.cfg
	$(MAKE) -f Makefile.OCaml $@

# Though not ideal, recursive make is needed here, in the name of portability.
#
# Directly including the freshly-generated Makefile.cfg could work with GNU
# Make (when the target filename in the rule is guarded by $(MAKE_RESTARTS)
# to prevent gmake from going into infinite loop), but fails with all other
# make implementations. Solaris make, BSD make and NMAKE all read Makefile.cfg
# before generating it (failing when *.cfg does not exist, reading stale
# contents otherwise). "-include" instead of "include" is not portable.


######################################################################
# For developers


######################################################################
# Tags

ETAGS=etags

# In Windows, tags and TAGS are the same, so make tags stops working
# after the first invocation.  The .PHONY declaration makes it work
# again.
.PHONY: tags
tags:
	@-if command -v $(ETAGS) > /dev/null ; then \
	    $(ETAGS) *.mli */*.mli *.ml */*.ml */*.m *.c */*.c *.txt \
		     *Makefile* \
	  ; fi

TAGS: tags
