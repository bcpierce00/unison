-include ../src/Makefile.ProjectInfo

all: $(NAME).1

../src/$(NAME)$(EXEC_EXT):
	$(MAKE) -C ../src UISTYLE=text

$(NAME).1: $(NAME).1.in opt_short.tmp opt_full.tmp
	sed -e '/@OPTIONS_SHORT@/r ./opt_short.tmp' \
		-e '/@OPTIONS_SHORT@/d' \
		-e '/@OPTIONS_FULL@/r ./opt_full.tmp' \
		-e '/@OPTIONS_FULL@/d' $(NAME).1.in > $(NAME).1

# Listing of preferences
opt_short.tmp: ../src/$(NAME)$(EXEC_EXT)
	../src/$(NAME)$(EXEC_EXT) -prefsman short > opt_short.tmp

opt_full.tmp: ../src/$(NAME)$(EXEC_EXT)
	../src/$(NAME)$(EXEC_EXT) -prefsman full > opt_full.tmp

clean:
	$(RM) *.tmp $(NAME).1
