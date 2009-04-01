# cabal2macpkg #

This is (will be) a tool to turn cabal libraries into Macintosh
packages.

## Status ##

Cabal2macpkg currently will build a .pkg file from a .cabal
library. The next step (not started yet) will be to provide a mode
that will read a .cabal file, recursively make .pkg files out of all
of its dependencies, and package them all up into a .mpkg file.

### Rough outline of the .pkg process ###

1. find a .cabal file in the current working directory
2. run "cabal build; cabal haddock" into a staging area
3. run "cabal register --gen-script" to generate a registration
   script that will be run by the OS X installer
4. turn the staging area into a mac package file using the OS X
   developer tools

A consequence of this quick n' dirty approach is that in order to
build the installer for a cabal package, you need to have already
installed all of its dependencies on the build machine.
