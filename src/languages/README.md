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


It might be totally possible, that above gettext from how-to is too C-like, and ocaml-gettext is doing stuff differently
Makefile on its GH page suggest that is the case - and it seems to be preferred, due to ocaml support added (as a patch) to normal gettext.

Eventually also one has to install compiled `*.mo` files somewhere, system-wide?
Systrace shows it searches for files `~/.opam/default/lib/gettext/share/locale/pl/LC_MESSAGES/` or `/usr/share/locale/pl/LC_MESSAGES`, but it can surely vary between OSes and such.
