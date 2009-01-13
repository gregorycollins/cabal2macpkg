TODO for cabal2macpkg
=====================

(in rough order)

round 1
-------

* parse the .cabal files to get important info about the package
  (name, version, etc)

* get the staging area in the right format for the OSX installer tool
  and run it 

* add --output=[FILE] and an --outputdir=[DIR] to control where the
  generated .pkg files go

* code cleanup (warnings, imports, formatting, etc)


round 2
-------

* add a "--metapackage" flag (or something similar) that will cause
  cabal2macpkg to make .pkg files of the project's dependencies and
  generate an aggregate .mpkg file

* may need "--exclude"/"--include" flags for this, or some other
  mechanism to stop the recursion
