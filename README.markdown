cabal2macpkg
============

This is (will be) a tool to turn cabal libraries into Macintosh
packages. Cabal2macpkg is in its very early stages, and doesn't work
yet, sorry.

Status
------
Rough outline of the process:

1. find a .cabal file in the current working directory
2. run "cabal build; cabal haddock" into a staging area
3. run "cabal register --gen-script" to generate a registration
   script that will be run by the OS X installer
4. turn the staging area into a mac package file using the OS X
   developer tools

I've gotten up to number three here.

A consequence of this quick n' dirty approach is that in order to
build the installer for a cabal package, you need to have already
installed all of its dependencies on the build machine.


TODO/Requirements
-----------------

First round:
* get it generating a .pkg file for an individual cabal library
* .pkg contents should be installable into any given prefix

Next round:
* Get it building a .mpkg metapackage from the haskell platform
  toplevel project
