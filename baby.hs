{-
  This document is a sort of scratchpad for my baby steps into the Haskell programming language.

  I am following the "Learn You A Haskell for Great Good!" book as a tutorial.  It is available for free at learnyouahaskell.com/chapters

  Use `ghci` to start the Haskell interpreter in the command line

  Use `:l baby` when inside ghci to load this file and get access to all of its functions and variables

-}

-- First haskell function!
doubleMe x = x + x

-- Second haskell function!
doubleUs x y = x*2 + y*2

-- Nested function calls!
doubleWe x y = doubleMe x + doubleMe y

-- A multi-line if then function
-- the "then" case is required in haskell
doubleSmallNumber x = if x > 100
			then x
			else x*2

-- A similar function as before on a single line
-- The use of ' in a function name is perfectly acceptable
-- Typically used to denote a strict (non-lazy) version of a function
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- Here's another function that returns a string
-- Strings are actually lists of ['c','h','a','r','a','c','t','e','r','s']
conanO'Brien = "It's a-me, Conan O'Brien!"

{- LISTS  -}

-- First haskell list!  Most common datastructure in haskell
-- Lists are homogeneous (they can only contain one data type)
lostNumbers = [4,8,15,16,23,42]


-- This function append x to y
append x y = x ++ y

-- This function prepends x to y
prepend x y = x : y

-- [1,2,3] is syntactic sugar for 1:2:3:[]

-- This function will get the element in index i from list l
getItem l i = l !! i
