# Internationalization

## Quick How-to

1. extracted with `xgettext` strings are put to `unison.pot` template, or it is created manually
   In theory running `cd src && xgettext -o languages/unison.pot --from-code=UTF-8 --keyword=_ --keyword=gettext *.ml` should do the trick, but it doesn't ;)
2. just once, language file is created by `msginit --input=unison.pot --locale=pl_PL --output=locales/pl/LC_MESSAGES/unison.po` for each language (pl in this case)
3. translation is compiled to mo file `msgfmt locales/pl/LC_MESSAGES/unison.po`
4. to update translation when `unison.pot` changes - run `msgmerge --update locales/pl/LC_MESSAGES/unison.po unison.pot` and repeat step 3

Each time new string is added/removed/changed, it should be added to `unison.pot` and translation files (`??.po`) should be regenerated and commited to repository.
Makefile inside this directory is expected to facilitate this.

## How to test?

After building, ensure that `/usr/share/locale/pl/LC_MESSAGES` or `/usr/local/share/locale/pl_PL/LC_MESSAGES` contains unison.mo file.
If not - you may need to copy it there manually.

[ ] I guess this might be a step in build, though i wonder if one can choose not to do it, and run from build dir like currently.

There are multiple other search paths where file could be, and where it is expected to be found, one can observe this with `strace ./unison`
If the file is in one of expected paths, `LANGUAGE=pl ./unison` or `LANGUAGE=pl ./unison -help` should result in running unison using translations.
Lo and behold, it has ONE string translated, this is PoC after all (look at `-version` description)
If your system's locale is English - you shouldn't need to pass `LANGUAGE=..`, but if your locale is for example FR, then it should fallback to default EN

If a translation (string) is not found for given string ID inside respective (for example) "fr.po" - a translation ID (typically being English text) is used;
however if a key is "WelcomeTextSomething" then, although it is "English" - it's not quite user friendly - this applies to all languages, including English.
Care would need to be taken to always update `unison.pot` template, update `en.po`, and other "po" files
(done automatically, translation strings are either empty, or copied from "pot" template).

When `unison` is distributed as a package (like a deb), package maintainer would need to also genrate "mo" files, and bundle them together inside the package.


## TODO

[x] figure out, if the project even wants translations at all - as on mailing list, project does not use issues in a typical way

[ ] if yes - discuss approach, is ocaml-gettext desired external dependency, being 0.4.2, or other apporaches are preferred

[ ] alternatively one could have a huge mapping and select message based on some variable, but i guess gettext is more standard approach

[ ] figure out what is the deal with generated `strings.ml`. While code contains normal strings, which could be extracted
    this seems to be generated, soo translate in generator or wut.

[ ] verify if the approach could work in other architectures/platforms, which project supports. Maybe gettext is not present there?

[ ] if approach is somewhat acceptable, improve `initial_extractor.py` as for now it captures over 1 MiB strings...
    (the process is way smoother with xgettext but first the translation keywords have to be present in the code...)

[ ] refactor Makefile + readme for preparing translations - naming and UX is poor. Actually either use ocaml patched extraactor or not and use base tools

[ ] figure out the overhead translation creates, and is there a desire to have a switch disabling this totally, to reduce bin size, or for other reasons

[ ] actually do the extraction and translation...

[ ] enable running without installing translations system-wide, and also add a target to install/update translations (mo files, locally or system-wide)


## BUGS

[ ] make is re-run in another terminal inside already build unison, without OPAM env initialized: it might complain about compilers mismatch

[ ] workdir is clean, make is run either without opam environment initialized or opam installed, but uses system OCaml and system libraries - it will build until failing to find opam package

[ ] while `unison` (cmdline) works, it seems `unison-gui` segfaults
    updated, backtrace in the code, linked to either how unison calculates size for window OR a bug in lablgtk
    it was discussed to drop lablgtk anyways: github.com/bcpierce00/unison/issues/1075
    When it does crash, stracktrace is as follows:
    ```
        Adding  `~text: (TranslateLib.s_ "TranslationTestString2")` to uigtk3.ml    crashes in runtime with:

	Thread 1 "unison-gui" received signal SIGSEGV, Segmentation fault.
	caml_darken (v=0, ignored=0x0, state=0x0) at major_gc.c:761
	warning: 761   major_gc.c: No such file or a directory
	(gdb) where
	#0  caml_darken (v=0, ignored=0x0, state=0x0) at major_gc.c:761
	#1  0x0000555555b97990 in caml_darken (state=state@entry=0x0, v=<optimized out>, ignored=ignored@entry=0x0) at major_gc.c:759
	#2  0x0000555555b9aca0 in write_barrier (obj=140737315273400, obj@entry=93825004394232, field=0, old_val=<optimized out>, new_val=93825004394224) at memory.c:140
	#3  caml_initialize (fp=fp@entry=0x7ffff5aefab8, val=val@entry=93825004394224) at memory.c:212
	#4  0x0000555555b867b5 in Val_GObject (p=0x5555560ed6f0) at ml_gobject.c:61
	#5  0x0000555555b82409 in ml_gtk_simple_callback (w=<optimized out>, data=0x7fffffffdac8) at ml_gtk.c:665
	#6  0x00007ffff7731be5 in ??? () at /lib/x86_64-linux-gnu/libgtk-3.so.0
	#7  0x0000555555b84f21 in ml_gtk_container_forall (w=140737319907464, clos=<optimized out>) at ml_gtk.c:678
	#8  0x0000555555bbaa0b in <signal handler called> ()
	#9  0x0000555555aade85 in camlGContainer__fun_2261 () at src/gContainer.ml:60
	#10 0x00005555558f2c2e in camlUigtk3__calcWinSize_5163 () at src/uigtk3.ml:3379
	#11 0x00005555558eec4c in camlUigtk3__createToplevelWindow_4876 () at src/uigtk3.ml:3420

        it appears that some size calculations are done in L3397
    ```

    Lovely. So, when building with gettext disabled, with stuff from opam:
    ```
    opam list
    lablgtk3          3.1.5-1                OCaml interface to GTK+3
    ocaml             4.12.0                 The OCaml compiler (virtual package)
    ```
    it builds, but crashes gui (tui works) - that means, this crash is NOT related to any changes with GETTEXT

    When built with `OCaml 5.3 + lablgtk3 3.1.5-1+b3` (system pkg) - it works (builds both via `dune build` and `make`)
      However, due to constrains on gettext (0.4.2 required) - it cannot use internationalization.
    When built with Ocaml 5.3.0 (via OPAM switch) `dune build` is unable to link unison_lib.a (camlSystem_win.stat_927 and similar), `make` works
      However, trying to build with 5.3.0 and gettext 0.5.0 (that is upstream HEAD)


It might be totally possible, that above gettext from how-to is too C-like, and ocaml-gettext is doing stuff differently
Makefile on its GH page suggest that is the case - and it seems to be preferred, due to ocaml support added (as a patch) to normal gettext.

Eventually also one has to install compiled `*.mo` files somewhere, system-wide?
Systrace shows it searches for files `~/.opam/default/lib/gettext/share/locale/pl/LC_MESSAGES/` or `/usr/share/locale/pl/LC_MESSAGES`, but it can surely vary between OSes and such.
