# Installing Unison

## Pre-built binaries

The easiest and quickest way is to install pre-built binaries available at many
package repositories. The caveat: some repositories are slow to update and
often include a very old version of Unison.

Alternatively, some pre-built binaries are made available at
https://github.com/bcpierce00/unison/releases for macOS, Linux and Windows.


## Building from source

### Package repositories

Many package repositories, including source-based repositories like `pkgsrc`,
make it easy to build from source by handling all the dependencies for you.
Please refer to instructions provided by the repository.


### Unix-like OS (GNU/Linux, BSDs, macOS, illumos-based OS, Solaris, ...)

#### Build prerequisites

- A recent version of OCaml compiler (version 4.08 at minimum) together with a
  C99 compiler (such as gcc, clang) -- see https://ocaml.org/
- GNU make
- Basic POSIX tools: sh, sed (optional, for manuals)

##### Optional, for the GUI only

- lablgtk3 and its prerequisites (ocamlfind, GTK 3 and its dependencies)

##### Optional, for building the user manual

- LaTeX
- dvips
- ps2pdf (included with Ghostscript, for example)
- HEVEA (https://hevea.inria.fr/) (optional, for HTML and text formats)
- Lynx (optional, for text format)
- (for developers only) HEVEA and Lynx are required to re-build the manual
  included in the Unison binary

#### Building

Building from source is as simple as executing:
```
make
```

Use `gmake` in environments where GNU make is not the default. If you are
using OPAM then `opam exec -- make` may work for you, as opam needs to set up
a specific environment.

Presence of lablgtk3 is detected automatically. If you want to force building
the GUI (or not), type `make UISTYLE=gtk3` or `make UISTYLE=text`.

There is currently no installation provided by the makefile. You can just copy
the built binaries to where you need them. The following files are produced:
```
src/unison              (the main executable)
src/unison-fsmonitor    (optional, on some build platforms)
src/fsmonitor.py        (optional, if unison-fsmonitor is not built)
man/unison.1            (optional, manual page)
doc/unison-manual.*     (optional, user manual in different formats)
```

#### Building all from scratch

It is very easy to build both the OCaml compiler and Unison from source without
using a package repository. Building OCaml has very few prerequisites. If your
system has a supported C compiler installed then the following may work out of
the box:

```
# Build OCaml compiler
## In a dir with extracted OCaml source
./configure
make
make install    # you may need elevated privileges

# Build unison
## In a dir with extracted Unison source
make
```

Building the GTK GUI this way is difficult as both GTK 3 and lablgtk3 must be
built and installed.


### macOS

#### Build prerequisites

- Xcode Command Line Tools (optional, for the native GUI)

#### Building

For the text user interface and GTK GUI, follow the Unix instructions above.

To build the macOS native GUI, execute:
```
make UISTYLE=mac
```

The built application will be located at `src/uimac/build/Default/Unison.app`.


### Windows

Building on Windows is currently somewhat complicated. All methods require
Cygwin as a POSIX-like layer for Windows. Cygwin is required for the build
process only; the build can produce fully native Windows binaries that don't
require Cygwin to run.

Builds are possible with MS Visual C++ (MSVC) (currently untested and likely
not working), MinGW-w64 (currently the best option) and Cygwin GNU C (untested;
produced binaries require Cygwin to run).

Tradeoff?

- MSVC and MinGW can produce statically linked Unison executable.
- The Cygwin GNU C option requires only free software.

You have to add `OSTYPE=$OSTYPE` as argument to `make`: `make OSTYPE=$OSTYPE`
(OSTYPE is preset to 'cygwin' by Cygwin).


### Build options

There are some additional options that control the build process:

- NATIVE: If you can't compile a native binary for your platform then add
  `NATIVE=false` as argument to `make`. This will produce a single native
  executable with OCaml runtime and Unison bytecode embedded.
- STATIC: Adding `STATIC=true` as argument to `make` will produce a (mostly)
  statically linked executable. This may not work on all platforms or with all
  build methods.
