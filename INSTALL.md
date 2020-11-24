# Installing `unison`

## Install pre-built binary

Installation instructions via package manager for various platforms are maintained in ["Downloading Unison"](https://github.com/bcpierce00/unison/wiki/Downloading-Unison) within the [repository wiki](https://github.com/bcpierce00/unison/wiki). Some older installation information is also available within the [*Installation*](https://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html#install) section of the [*Unison File Synchronizer ~ User Manual and Reference Guide*](https://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html) on the [`unison` website](https://www.cis.upenn.edu/~bcpierce/unison).

More simply for some, pre-built binary package archives for MacOS, Linux, and Windows are available from the [Releases](https://github.com/bcpierce00/unison/releases) section of the [`unison` repository](https://github.com/bcpierce00/unison), with further installation instructions in [CI Binary Instructions](https://github.com/bcpierce00/unison/wiki/CI-Binary-instructions).

## Build from source

> Last updated: 2019-01
>
> The instructions below are rather old, and there may be simpler ways now. For example, on some systems, just doing `opam install lablgtk` seems to be enough to get all the support libraries installed.

We are happy to announce a new version of Unison with a user interface based on Gtk 2.2, enabling display of filenames with any locale encoding.

### Linux (and maybe other Unixes)

In order to build `unison`,

> #### Optional requirements for GTK-UI
>
> - install glib, pango, gtk (version >2.2); from <http://www.gtk.org>
> - install lablgtk2 (version >20030423); from <http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgtk.html>

1. install unison (version >2.9.36); from <https://www.cis.upenn.edu/~bcpierce/unison>

    1. Simply type `make`.

    2. Makefile will detect the presence of lablgtk2 directory
    *$(OCAMLLIBDIR)/lablgtk2* (such as */usr/local/lib/ocaml/lablgtk2/*)
    and use UISTYLE=gtk2 by default. If absent, it falls back to
    lablgtk with UISTYLE=gtk, then back to UISTYLE=text.

    3. You can force the selection by using...

        - `make UISTYLE=gtk2`
        - or `make UISTYLE=gtk`
        - or `make UISTYLE=text`

2. setup your locale environment properly; for example, `export LANG=zh_HK.BIG5-HKSCS`

    otherwise, you will get

    ``` text
    Uncaught exception Glib.GError("Invalid byte sequence in conversion input")
    ```

3. enjoy unison with i18n!

### MacOS/OSX

> #### Optional requirements for GTK-UI (MacOS/OSX)
>
> - install gtk2 using fink (`sudo /sw/bin/fink install gtk+2`)
> - install lablgtk2 (version >20030423); from <http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgtk.html>

Otherwise, follow the same process from step 1 of the Linux instructions.

In our tests, the linker generates lots of error messages, but appears
to build a working executable.  Also, we have not yet been able to get
this build to work with 'STATIC=true'.

### Windows

Installation notes to build Unison on Windows systems

We provide two options for building Unison on MS Windows. Both
options require the Cygwin layer to be able to use a few GNU tools as
well as the OCaml distribution version. The options differ in the C
compiler employed: MS Visual C++ (MSVC) vs Cygwin GNU C.

Tradeoff?

- Only the MSVC option can produce statically linked Unison executable.
- The Cygwin GNU C option requires only free software.

The files [INSTALL.win32-cygwin-gnuc.mkd](./INSTALL.win32-cygwin-gnuc.mkd) and [INSTALL.win32-cygwin-msvc.mkd](./INSTALL.win32-cygwin-msvc.mkd) describe the building procedures for the respective options.
