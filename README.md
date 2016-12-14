# cghs

I have created (and abandoned) tons of different data types for my CG-related experiments (https://github.com/inariksit/cgsat, https://github.com/inariksit/cgexp). This is an attempt to build and maintain just one type, and use it in all of my projects.

This repository will contain

* Data type: src/Rule.hs
* Parser: BNFC grammars in src/bnfc, and transformation into Haskell data types in src/Parse.hs. The BNFC grammars are terrible, I just started writing it when I didn't think this would be my biggest academic achievement. Maybe one day I'll use the thing that Tino Didriksen uses.
* QuickCheck tests and generators. So you can test your stuff, and play a party game "was this CG rule written by a human or QuickCheck generator".