This directory contains the files for creating setups for Microsoft Windows using [Inno Setup](https://github.com/jrsoftware/issrc).

Building Unison setup
---------------------

* Download GTK 2 runtime and place it in directory `bin`
* Download Unison binaries from https://www.irif.fr/~vouillon/unison/ and place Unison GTK binary together with `unison-fsmonitor.exe` in directory `bin`
* Download `plink.exe` and `putty.exe` from https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html and place both in directory `bin`
* Open `Unison.iss` by Inno Setup, adjust version information and compile it
