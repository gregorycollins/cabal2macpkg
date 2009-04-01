TODO for cabal2macpkg
=====================

## TODO items (round 1) ##

* add support for relocatable packages -- not sure how to do this, any
  ideas would be appreciated. Manuel wanted this.


## TODO items (round 2) ##

* add a "--metapackage" flag (or something similar) that will cause
  cabal2macpkg to make .pkg files of the project's dependencies and
  generate an aggregate .mpkg file

* may need "--exclude"/"--include" flags for this, or some other
  mechanism to stop the recursion. For the haskell platform a
  recursion depth of 1 is probably fine.


## status update ##

N.B. this is more for my notetaking benefit than for the reader

Status: getting close. Have decided, for the time being, to use the
Tiger interface to packagemaker ("backwards-compatibility mode")
because information is easier to come by on its source formats, and
the command-line arguments are easier to grok. The downside of this
approach is that it builds "bundle files" (really directories) instead
of compressed files like Leopard does. You can also read this as "I
got impatient with my inability to get the Leopard approach to work".

If I can dig up more information about WTF packagemaker is expecting
in 10.5 mode, I'll try to abstract the backend to work with either. At
least one person asked for consideration for 10.4 anyways (although I
lack such a machine.)

I am really frustrated with the quality of Apple's technical
documentation in this area: there is little information about the
command-line tool in the Software Delivery Guide, the manpage doesn't
describe what's going on nearly well enough, several of the necessary
file formats seem to be completely undocumented... Not to mention: the
ad-hoc nature of the entire installation system (what are the package
receipts for? There are no programs that consume them!), no facility
for uninstallers (urk!!), the fact that they RUINED property lists by
going to XML, and this doozy:

    "Without proper care when specifying the ownership and access
     permissions of component files, it is possible to render a system
     unusable. Make sure you test all installer packages before
     shipping them to customers."

(Parenthetical: as far as I can tell from crystal-balling the scraps
of doco. that I did find, this scare quote isn't even true anymore, at
least unless you pass an explicit option to packagemaker...)

I would **kill** for a guide to a "hello, world!" app done with the
command-line tools right now. Apple's solution seems to be "use the
GUI tool!" which is useless to me.


