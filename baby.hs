{-
  This document is a sort of scratchpad for my baby steps into the Haskell programming language.

  I am following the "Learn You A Haskell for Great Good!" book as a tutorial.  It is available for free at learnyouahaskell.com/chapters

  Use `ghci` to start the Haskell interpreter in the command line

  Use `:l baby` to load this file and get access to all of its functions and variables

-}

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleWe x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
			then x
			else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

lostNumbers = [4,8,15,16,23,42]

glue x y = x ++ y
