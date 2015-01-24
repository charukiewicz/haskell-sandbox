{-
  This document is a sort of scratchpad for my baby steps into the Haskell programming language.

  I am following the "Learn You A Haskell for Great Good!" book as a tutorial.  It is available for free at learnyouahaskell.com/chapters

  Use `ghci` to start the Haskell interpreter in the command line

  Use `:l baby` when inside ghci to load this file and get access to all of its functions and variables

-}

{- FUNCTIONS -}

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

-- Infix functions:
--
-- Note that a function can be an infix function if it is surrounded by backticks (`) and placed in between its two parameters
-- For example, div 90 10 returns 9
-- But, 90 `div` 10 also returns 9

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

-- Lists have many parts
someList = [1,2,3,4,5]

-- The head of someList = 1
-- The tail of someList = [2,3,4,5]
-- The last of someList = 5
-- The init of someList = [1,2,3,4]

{- List Manipulation Functions

   length l 	-- takes a list and returns its length
   null l 	-- returns True if the list is empty, False otherwise
   reverse l 	-- reverses the list
   take n l 	-- returns a list of the first n elements from l
   drop n l 	-- returns a list ignoring the first n elements from l
   maximum l	-- returns the largest element in a list
   minimum l	-- returns the smallest element in a list
   sum l	-- returns the sum of the elements in a list
   product l	-- returns the product of the elements in a list
   elem n l	-- returns True if n is an element of l
-}

{- RANGES -}

-- We can write a list using ranges
-- [1,2,3,4,5] can bet written [1..5]

-- This function returns a list from range x to y
range x y = [x..y]

-- We can specify steps with ranges as well
-- [2,4,6,8,10] can be written [2,4..10]

-- The first 24 multiples of 13 can be written as [13,26..24*13]
-- This can also be written as: take 24 [13,26..]

{- List Range Functions

   cycle l	-- takes a list and repeats it to make an infite list
   repeat n	-- takes an element and makes an infinite list out of it
   replicate n k -- takes element k and makes a list of length n from it
-}

{- LIST COMPREHENSION -}

-- Haskell uses math-like set comprehension notation to build lists
-- The following uses this notation to build a list of the first ten even natural numbers
firstTenEven = [x*2 | x <- [1..10]]
