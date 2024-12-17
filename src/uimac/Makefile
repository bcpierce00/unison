# This makefile should not be used directly.
# It should always be called from ../Makefile
default:
	$(MAKE) -C .. macui
.PHONY: default

# Note: The OCaml library names changed starting with OCaml 5.1.0
.PHONY: macexecutable
macexecutable:
	( \
	  LIB_SUFFIX=$$(ocaml -e 'if Sys.ocaml_release.major > 5 || Sys.ocaml_release.major = 5 && Sys.ocaml_release.minor >= 1 then print_string "nat"' 2> /dev/null); \
	  printf "MARKETING_VERSION = $(VERSION)\nOCAMLLIBDIR = $(OCAMLLIBDIR)\nOCAMLLIB_UNIX = -lunix$${LIB_SUFFIX}\nOCAMLLIB_STR = -lcamlstr$${LIB_SUFFIX}" > ExternalSettings.xcconfig)
	xcodebuild $(XCODEFLAGS) SYMROOT=build
	$(CC) $(CFLAGS) cltool.c -o build/Default/Unison.app/Contents/MacOS/cltool -framework Carbon
	codesign --remove-signature build/Default/Unison.app
	codesign --force --sign - build/Default/Unison.app/Contents/MacOS/cltool
	codesign --force --sign - --entitlements build/uimac*.build/Default/uimac.build/Unison.app.xcent build/Default/Unison.app
	codesign --verify --deep --strict build/Default/Unison.app
# cltool was added into the .app after it was signed, so the signature is now
# broken. It must be removed, cltool separately signed, and then the entire
# .app (re-)signed.

.PHONY: clean
clean:
	xcodebuild clean 2> /dev/null || true
	-rm -f -r build ExternalSettings.xcconfig
