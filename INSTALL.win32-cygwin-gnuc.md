# Build/Install notes for `unison` on Windows (using Cygwin)

> Note: fully constructed and packaged executables for MacOS, Linux, and Windows are available in the [Releases](https://github.com/bcpierce00/unison/releases) section of the [`unison` repository](https://github.com/bcpierce00/unison).

<!-- markdownlint-disable ol-prefix -->
<!-- spell-checker:ignore markdownlint -->

<!-- spell-checker:ignore (abbrev/names) Cygwin MSVC MinGW lablgtk pango pixbuf -->
<!-- spell-checker:ignore (libraries) ADVAPI ADVAPI32 libatk libbz libcairo libexpat libffi libfontconfig libfreetype libgcc libgdk libgio libglib libgmodule libgobject libgtk libharfbuzz libintl liblgtk libpango libpangocairo libpangoft libpangowin libpcre libpixman libpng libwinpthread msvcrt -->
<!-- spell-checker:ignore (options) alpe sandboxing sjlj -->
<!-- spell-checker:ignore (shell) cygcheck cygdrive cygpath diffutils esac eval mkdir objdump opam popd pushd rsync xargs WINDIR -->
<!-- spell-checker:ignore (jargon) toolset toolchain executables -->
<!-- spell-checker:ignore () OSARCH OSTYPE UISTYLE cygwinports depext devel fsmonitor rwxr -->

> Last updated: 2020-11
>
> The current instructions were tested on the Windows 10 platform, using Cygwin v3.1.6 and OCaml v4.08.1, for `unison` v2.51.3.
>
> Additional Windows-specific OCaml information is available from [OCaml for Windows](https://fdopen.github.io/opam-repository-mingw), including [installation using Cygwin](https://fdopen.github.io/opam-repository-mingw/installation).

## Contents

1. Setting up the Windows system
    - 1.1 General requirements
    - 1.2 Cygwin
    - 1.3 OCaml

2. Building `unison`

## Setting up the Windows system

### 1.1 · General requirements

For a complete installation from scratch, you will need about 2 GiB of storage (almost all of that space is used for the Cygwin installation).

Cygwin, a unix-like layer, is required in order to make use of the GNU toolset (including 'bash', 'make', 'sed', 'patch', etc).

The Cygwin port of OCaml distribution is required.

### 1.2 · Cygwin

1. Download the current 64-bit [Cygwin](https://www.cygwin.com) installer ([setup-x86_64.exe](https://www.cygwin.com/setup-x86_64.exe)) from ["Install Cygwin"](https://cygwin.com/install.html) on the Cygwin website.

> The 64-bit installer can build both 32-bit and 64-bit unison binaries. The 32-bit installer can also be used for 32-bit machines if necessary (by using ([setup-x86.exe](https://cygwin.com/setup-x86.exe)) and using `set "CYGWIN_ARCH=x86"` below).

2. Install Cygwin with the required packages.
    - Basic development toolchain package prerequisites are *curl*, *diffutils*, *dos2unix*, *git*, *m4*, *make*, *mercurial*, *patch*, *perl*, *rsync*, *unzip*, and *mingw64-i686-gcc-core*; for 64-bit target builds, *mingw64-x86_64-gcc-core* is also required.
    - For GTK/GUI builds (using `lablgtk`), the additional prerequisite packages are *mingw64-i686-glib2.0*, *mingw64-i686-gtk2.0*, *mingw64-i686-pango1.0*, *libgtk2.0_0*, *libgtk2.0_devel*, *ocaml-lablgtk2*; for 64-bit target builds, *mingw64-x86_64-glib2.0*, *mingw64-x86_64-gtk2.0*, *mingw64-x86_64-pango1.0* are also required.

> Use the Windows shell (`cmd` or `PowerShell`).

```batch
@rem # CMD shell

set "CYGWIN_ARCH=x86_64" &@rem # [x86,x86_64]
set "CYGWIN_ROOT=c:\cygwin"
set "CYGWIN_URL=http://cygwin.mirror.constant.com"

@rem # basic set of default packages for the Cygwin/OCaml toolchain
set "PACKAGES=curl,diffutils,dos2unix,git,m4,make,mercurial,patch,perl,rsync,unzip,mingw64-i686-gcc-core"
if /i "%CYGWIN_ARCH" == "x86_64" set "PACKAGES=%PACKAGES%,mingw64-x86_64-gcc-core"

@rem # 'lablgtk' prerequisite packages
set "PACKAGES=%PACKAGES%,mingw64-i686-glib2.0,mingw64-i686-gtk2.0,mingw64-i686-pango1.0,libgtk2.0_0,libgtk2.0_devel,ocaml-lablgtk2"
if /i "%CYGWIN_ARCH" == "x86_64" set "PACKAGES=%PACKAGES%,mingw64-x86_64-glib2.0,mingw64-x86_64-gtk2.0,mingw64-x86_64-pango1.0"

@rem # install Cygwin + packages
@rem # * `setup-%CYGWIN_ARCH%.exe` can be downloaded from https://cygwin.com/install.html
setup-%CYGWIN_ARCH%.exe --no-shortcuts --quiet-mode --root "%CYGWIN_ROOT%" --site "%CYGWIN_URL%" --packages %PACKAGES%
```

## 1.3 · OCaml (via `opam`)

> Use the Cygwin `bash` shell (via `"%CYGWIN_ROOT%"\Cygwin.bat`).

1. Install and configure [`opam`](https://opam.ocaml.org) (the OCaml Package Manager).

```bash
# Cygwin bash shell

BUILDER_ARCH=64 ## [32,64] ; note: should match the bit size of the selected CYGWIN_ARCH (above)
DOWNLOADS_DIR=/tmp/ocaml-downloads
OPAM_REPOSITORY_URL=https://github.com/fdopen/opam-repository-mingw.git#opam2

# * `opam` installation/setup
mkdir -p "${DOWNLOADS_DIR}"
pushd "${DOWNLOADS_DIR}"
curl -#L https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam${BUILDER_ARCH}.tar.xz -o "${DOWNLOADS_DIR}/opam${BUILDER_ARCH}.tar.xz"
tar xvf "opam${BUILDER_ARCH}.tar.xz"
bash opam${BUILDER_ARCH}/install.sh
popd
opam init --disable-sandboxing --enable-completion --enable-shell-hook --auto-setup default "${OPAM_REPOSITORY_URL}"
opam update
```

2. Install and configure OCaml (using `opam` switches).

```bash
# Cygwin bash shell

OCAML_VERSION=4.08.1
TARGET_ARCH=32 ## [32,64]
TOOLSET=mingw ## [mingw,msvc] ; note: the 'MSVC' toolset has GTK+/lablgtk installation blockers

SWITCH=${OCAML_VERSION}+${TOOLSET}${TARGET_ARCH}

opam switch create ${SWITCH} ${SWITCH}c ## using '...c' pre-compiled version decreases switch installation time by approximately 50%
opam switch ${SWITCH}
eval "$(opam env)"
case "${TOOLSET}" in "mingw")
  ocaml-env exec -- opam install --yes depext-cygwinports depext
  ocaml-env exec -- opam install --yes lablgtk
  ;; esac
case "${TOOLSET}" in "msvc")
  ocaml-env exec -- opam install --yes depext
  ##ocaml-env exec -- opam install --yes lablgtk ## doesn't work
  ;; esac
```

## Building `unison`

Download the `unison` source code from the [repository on GitHub](https://github.com/bcpierce00/unison.git) using `git`.
Then, build `unison` using `make`.

> Use the Cygwin `bash` shell (via `"%CYGWIN_ROOT%"\Cygwin.bat`).

### Build the text-UI `unison`

```bash
# Cygwin bash shell

PROJECT_DIR=~/Projects/unison
UNISON_REPO_URL=https://github.com/bcpierce00/unison.git
UNISON_REPO_COMMIT=master

mkdir -p "${PROJECTS_DIR}"

## * unison source
git clone "${UNISON_REPO_URL}" "${PROJECT_DIR}/repo"
cd "${PROJECT_DIR}/repo"
git checkout ${UNISON_REPO_COMMIT} -f ## optional

## * build unison UISTYLE=text (aka the text-UI `unison`)
## (note: OSTYPE is preset to 'cygwin' by Cygwin)
make src OSTYPE=$OSTYPE UISTYLE=text
strip src/*.exe ## optional
```

This process builds the text-UI versions of `unison.exe` and `unison-fsmonitor.exe`, which are available in the *src* subdirectory.

These files are "statically linked", without any Cygwin dependency, and may be used without need for any additionally installed files on any Windows platform (at least for WinXPsp3+). Note that "statically linked" in the Windows world doesn't technically mean that the executable has no external references but that any required external DLLs are already built into the Windows platform. This is functionally the same as a fully statically linked executable.

> ### Build highlights for the text-UI `unison`
>
> ```bash
> $ make src OSTYPE=$OSTYPE UISTYLE=text
> ...
> UISTYLE = text
> Building for Windows
> NATIVE = true
> THREADS = false
> STATIC = false
> OSTYPE = cygwin
> OSARCH = win32
> ...
> $ ls -Al src/*.exe
> -rwxr-xr-x+ 1 user None 4542993 Nov  9 20:54 src/unison.exe
> -rwxr-xr-x+ 1 user None 2842929 Nov  9 20:54 src/unison-fsmonitor.exe
> $ strip src/*.exe
> $ ls -Al src/*.exe
> -rwxr-xr-x+ 1 user None 2282496 Nov  9 23:47 src/unison.exe
> -rwxr-xr-x+ 1 user None 1023488 Nov  9 23:47 src/unison-fsmonitor.exe
> $ dll_refs() { eval "$(opam env)" ; eval "$(ocaml-env cygwin)" ; objdump -p "$@" | grep -Po "\S+[.]dll$" | xargs -I{} 2>/dev/null which '{}' | sort -u ; }
> $ dll_refs src/*.exe
> /cygdrive/c/WINDOWS/system32/ADVAPI32.dll
> /cygdrive/c/WINDOWS/system32/KERNEL32.dll
> /cygdrive/c/WINDOWS/system32/msvcrt.dll
> /cygdrive/c/WINDOWS/system32/SHELL32.dll
> /cygdrive/c/WINDOWS/system32/VERSION.dll
> /cygdrive/c/WINDOWS/system32/WS2_32.dll
> ```

### Build the GTK-UI `unison`

The GTK-UI `unison` is similarly built using simply `make src OSTYPE=$OSTYPE`. Again the resultant executables are available within the *src* directory.

The GTK-UI executables have no Cygwin DLL dependencies but using GTK brings with it quite a few direct and even more secondary DLL dependencies (listed below in the "Build highlights"). Additionally, to be usable and portable to other host PCs, GTK applications have additional structural packaging requirements in order to be usable on other hosts. Further details are available in the next section.

> ### Build highlights for the GTK-UI `unison`
>
> ```bash
> $ make src OSTYPE=$OSTYPE
> ...
> UISTYLE = gtk2
> Building for Windows
> NATIVE = true
> THREADS = false
> STATIC = false
> OSTYPE = cygwin
> OSARCH = win32
> ...
> $ ls -Al src/*.exe
> $ dll_refs() { eval "$(opam env)" ; eval "$(ocaml-env cygwin)" ; objdump -p "$@" | grep -Po "\S+[.]dll$" | xargs -I{} 2>/dev/null which '{}' | sort -u ; }
> $ filtered_dll_refs() { list="$(dll_refs "$@" | grep -vF "$(cygpath ${WINDIR})" | perl -alpe '$_ = qq/@{[sort @F]}/')" ; echo -n "$list" ; }
> $ recursive_dll_refs() { list="$(dll_refs "$@")" ; n=0 ; while [ $n -lt $(echo "$list" | wc -l) ]; do n=$(echo "$list" | wc -l) ; list="$(dll_refs $list)" ; done ; echo -n "$list" ; }
> $ recursive_filtered_dll_refs() { list="$(filtered_dll_refs "$@")" ; n=0 ; while [ $n -lt $(echo -n "$list" | wc -l) ]; do n=$(echo -n "$list" | wc -l) ; list="$(filtered_dll_refs $list)" ; done ; echo -n "$list" ; }
> $ dll_refs src/*.exe
> /cygdrive/c/WINDOWS/system32/ADVAPI32.dll
> /cygdrive/c/WINDOWS/system32/KERNEL32.dll
> /cygdrive/c/WINDOWS/system32/msvcrt.dll
> /cygdrive/c/WINDOWS/system32/SHELL32.dll
> /cygdrive/c/WINDOWS/system32/VERSION.dll
> /cygdrive/c/WINDOWS/system32/WS2_32.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgdk_pixbuf-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgdk-win32-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libglib-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgobject-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgtk-win32-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpango-1.0-0.dll
> $ recursive_filtered_dll_refs src/*.exe
> /usr/i686-w64-mingw32/sys-root/mingw/bin/iconv.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libatk-1.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libbz2-1.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libcairo-2.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libexpat-1.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libffi-6.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libfontconfig-1.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libfreetype-6.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgcc_s_sjlj-1.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgdk_pixbuf-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgdk-win32-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgio-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libglib-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgmodule-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgobject-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libgtk-win32-2.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libharfbuzz-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libintl-8.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpango-1.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpangocairo-1.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpangoft2-1.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpangowin32-1.0-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpcre-1.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpixman-1-0.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libpng16-16.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/libwinpthread-1.dll
> /usr/i686-w64-mingw32/sys-root/mingw/bin/zlib1.dll
> ```

#### Packaging GTK-UI `unison`

Statically-linked Windows applications using GTK+ are "not supported" by the [GTK project](https://www.gtk.org) for reasons (for discussions, see [GTK-list~static-linking-in-GTK-windows] and [SO~statically-linking-GTK-in-windows]). It is possible to create a completely statically linked Windows executable but it requires a custom build of GTK+ (details at [Static GTK+3 for Windows][Static-GTK+3-for-Windows]); but that process is currently beyond the scope of this project.

[SO~statically-linking-GTK-in-windows]: https://stackoverflow.com/questions/1875855/statically-linking-gtk-libraries-in-windows
[GTK-list~static-linking-in-GTK-windows]: https://mail.gnome.org/archives/gtk-app-devel-list/2003-September/msg00027.html
[Static-GTK+3-for-Linux]: http://www.tarnyko.net/en/?q=node/29
[Static-GTK+3-for-Windows]: http://www.tarnyko.net/en/?q=node/31

So, for portability between Windows hosts, the GTK-UI `unison` executable must be packaged with all dependencies and into a specific directory structure. The GTK project explains the requirements at [GTK ~ Setting up GTK for Windows][GTK~windows-building-distributing].

[GTK~windows-building-distributing]: https://www.gtk.org/docs/installations/windows#building-and-distributing-your-application

1. The GTK-UI `unison` executable should be placed into a subdirectory named *bin* within TARGET_DIR.
2. Find all required DLLs (filtering out any DLLs already included with Windows) and copy those DLLs into the same *bin* directory.

```bash
# Cygwin bash shell
TARGET_DIR=...
# find required DLLs
dll_refs() { eval "$(opam env)" ; eval "$(ocaml-env cygwin)" ; objdump -p "$@" | grep -Po "\S+[.]dll$" | xargs -I{} 2>/dev/null which '{}' | sort -u ; }
filtered_dll_refs() { list="$(dll_refs "$@" | grep -vF "$(cygpath ${WINDIR})" | perl -alpe '$_ = qq/@{[sort @F]}/')" ; echo -n "$list" ; }
recursive_dll_refs() { list="$(dll_refs "$@")" ; n=0 ; while [ $n -lt $(echo "$list" | wc -l) ]; do n=$(echo "$list" | wc -l) ; list="$(dll_refs $list)" ; done ; echo -n "$list" ; }
recursive_filtered_dll_refs() { list="$(filtered_dll_refs "$@")" ; n=0 ; while [ $n -lt $(echo -n "$list" | wc -l) ]; do n=$(echo -n "$list" | wc -l) ; list="$(filtered_dll_refs $list)" ; done ; echo -n "$list" ; }
IFS=$'\n' DLL_list=( "$(recursive_filtered_dll_refs "${TARGET_DIR}"/bin/*.exe)" )
# copy DLLs into *bin*
for dll in ${DLL_list[@]} ; do cp "${dll}" "${TARGET_DIR}"/bin ; done
```

3. Copy the minimally needed library support files from the build host into a subdirectory named *lib* within the TARGET_DIR (*lib* should be a sibling directory to *bin*). For the GTK-UI `unison` build, gdk-pixbuf-2.0 is the only required library; the TARGET_ARCH_ID must match the target host... . Plus, must rewrite the pixbuf *loaders.cache* file to point at the new relative location.

```bash
# Cygwin bash shell
# [GTK ~ Windows/Build and Distribution] ref: https://www.gtk.org/docs/installations/windows#building-and-distributing-your-application
# [GTK+ Tutorial (for Windows)] ref: http://www.tarnyko.net/repo/gtk3_build_system/tutorial/gtk3_tutorial.htm
# [gtk-rs ~ cross platform build/bundling] ref: https://gtk-rs.org/docs-src/tutorial/cross
# ... + gdk-pixbuf; ref: https://github.com/gtk-rs/gtk/issues/422#issuecomment-686927113
TARGET_ARCH_ID=...
# * copy gdk-pixbuf lib/dlls
cp -r /usr/${TARGET_ARCH_ID}-w64-mingw32/sys-root/mingw/lib/gdk-pixbuf-2.0 "${TARGET_DIR}"/lib/
# * update loader.cache to point to local relative installation
mv "${TARGET_DIR}"/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache "${TARGET_DIR}"/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache.original
cat "${TARGET_DIR}"/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache.original | sed -E 's#([^"]*)(lib/gdk-pixbuf-2.0/2.10.0/loaders/[^"]*[.]dll)#../\2#' > "${TARGET_DIR}"/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
```
