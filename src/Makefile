# Unison file synchronizer: src/Makefile
# Copyright 1999-2023 (see ../LICENSE for terms).

## User Settings

# Set NATIVE=false if you are not using the native code compiler (ocamlopt)
# This is not advised, though: Unison runs much slower when byte-compiled.
NATIVE=true

########################################################################
########################################################################
#     (There should be no need to change anything from here on)       ##
########################################################################

######################################################################
# Building installation instructions

.PHONY: all
all: tui guimaybe macuimaybe fsmonitor

########################################################################
## Miscellaneous developer-only switches
# [2023-05] This check is here only temporarily for a smooth transition
ifeq ($(STATIC), true)
  $(error Variable STATIC is no longer in use. Please set appropriate LDFLAGS,\
    like -static, instead)
endif

# NAME, VERSION, and MAJORVERSION, automatically generated
-include Makefile.ProjectInfo

########################################################################
### Compilation rules

include Makefile.OCaml

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

######################################################################
# Misc

.PHONY: clean

####################################################################
# Documentation strings

# Cons up a fake strings.ml if necessary (the real one is generated when
# we build the documentation, but we need to be able to compile the
# executable here to do that!)
strings.ml:
	echo "(* Dummy strings.ml *)" > strings.ml
	echo "let docs = []" >> strings.ml

