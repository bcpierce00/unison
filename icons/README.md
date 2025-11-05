# Unison icons

This directory contains icons for use with unison.

# Known purposes

## Unix Desktop Environments

The square icon (U.svg) could be installed in share/icons/hicolor for
use by desktop environments.

The
[icon theme specification](https://specifications.freedesktop.org/icon-theme/latest/)
requires a raster format, preferring png, and optionally svg.
The minimal requirement is a 48x48 icon for hicolor.

## Windows

Windows needs a
[`.ico`](https://en.wikipedia.org/wiki/ICO_(file_format))file.
Currently one is present in src/win32rc, but there is no information
about where it came from or how it was created.

The command `icotool` from the package
[`icoutils`](https://www.nongnu.org/icoutils/) (last release in 2018)
appears to be able to create ico files and extract images from ico
files.

According to wikipedia, starting with Windows Vista, `png` is
acceptable within `ico`.

# Variants

U.svg is a square icon with just a U with arrows.

unison.svg uses the U (apparently from U.svg) and adds "nison" to make
a word icon.

\todo Consider renaming U to unison and unison to unison-word.  It
seems likely that the bits currently in U are most useful, and they
should have a more understandable installed name when installed.

# Build System Issues

Currently, there is no automated or scripted support to convert the
svg-format files to `png` or `ico`, and the derived formats are
checked in.  Further, the files within `U.ico` do not appear to all
have matching `.png` files.
