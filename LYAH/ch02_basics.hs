{-
  This document is a sort of scratchpad for my baby steps into the Haskell programming language.

  I am following the "Learn You A Haskell for Great Good!" book as a tutorial.  It is available for free at learnyouahaskell.com/chapters

  Use `ghci` to start the Haskell interpreter in the command line

  Use `:l basics` when inside ghci to load this file and get access to all of its functions and variables

-}

{-------------}
{- FUNCTIONS -}
{-------------}

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

{---------}
{- LISTS -}
{---------}

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

   length l 	- takes a list and returns its length
   null l		- returns True if the list is empty, False otherwise
   reverse l 	- reverses the list
   take n l 	- returns a list of the first n elements from l
   drop n l 	- returns a list ignoring the first n elements from l
   maximum l	- returns the largest element in a list
   minimum l	- returns the smallest element in a list
   sum l		- returns the sum of the elements in a list
   product l	- returns the product of the elements in a list
   elem n l		- returns True if n is an element of l
-}

{----------}
{- RANGES -}
{----------}

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

{----------------------}
{- LIST COMPREHENSION -}
{----------------------}

-- Haskell uses math-like set comprehension notation to build lists
-- The following uses this notation to build a list of the first ten even natural numbers
firstTenEven = [ x*2 | x <- [1..10] ]

evenTenThroughTwenty = [ x*2 | x <- [1..10], x*2 >= 12 ]

{- The function above have multiple parts -}
-- Output function: x*2
-- Variable: x
-- Input set: x <- [1..10]
-- Predicate: x*2 >= 12

-- The next functions finds all numbers from 50 to 100 whose remainder
-- when divded by 7 is 3:
fiftyThroughHundredModThree = [ x | x <- [50..100], x `mod` 7 == 3 ]

-- Replaces each odd number greater than 10 with "BANG!" and each
-- odd number that's less than 10 with "BOOM!".  If a number isn't
-- odd, throw it out of the list.
boomBang xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

-- We can have several predicates.  The following function grabs all
-- numbers from 10 to 20, excluding 13, 15, and 19.
tenThroughTwentyWithSkips = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] 

-- NOTE: An element must satisfy all predicates to be included in the output list

-- In addition to multiple predicates, we can also draw from several lists
multiplyTwoLists' = [ x*y | x <- [2,5,10], y <- [8,10,11]]

multiplyTwoLists l1 l2 = [ x*y | x <- l1, y <- l2 ]

-- We can use lists that contain strings as well
nouns = ["hobo", "frog", "pope"]
adjectives = ["lazy", "grouchy", "scheming"]
phraseMaker ns as = [a ++ " " ++ n | n <- ns, a <- as]

-- This function replaces every element in the input list with 1 and takes its sum.
-- "_" accepts any value and does not assign a variable name.
length' xs = sum [ 1 | _ <- xs ]

-- This function takes a string and removes everything except uppercase letters from it.
removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z']]

-- Nested list comprehension is also possible with this notation.  The following
-- function removes all odd numbers within nested input lists without flattening.
removeNestedOdds xxs = [ [ x | x <- xs, even x ] | xs <- xxs ]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

{----------}
{- TUPLES -}
{----------}

-- Tuples are some similarities to lists, but have specific differences which make them
-- applicable in different situations from lists.

-- Tuples are used when you know exactly how many values you want to combine.The type of
-- a tuple depends on how many components it has and the types of each of the respective
-- components. The components do NOT have to be homogenous; unlike a list, a tuple can
-- contain a combination of several types.

aPair = (1,2)					-- Type :: (Integer, Integer)
aTriple = (1,2,3)				-- Type :: (Integer, Integer, Integer)
anotherPair = ('a',1)			-- Type :: (Char, Integer)
anotherTriple = ("abc",'d',5)	-- Type :: ([Char], Char, Integer)

{- TUPLE FUNCTIONS

    fst t       - takes a pair (tuple) and returns the first element
    snd t       - takes a pair (tuple) and returns the second element
    zip l1 l2   - takes two lists and returns a list of pairs
-}

{-----------}
{- WRAP UP -}
{-----------}

-- Covered in this chapter:
--  Basic Haskell Operators
--  Functions
--  Lists
--  Ranges
--  List Comprehension
--  Tuples

-- Time for a combination of list comprehension and tuples.
-- The following expression will get a list of all possible triangle
-- lengths whose sides are all of length 1 through 10.
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

-- Now the following will only list right triangles whose sides are
-- less than or equal to 10
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- The following will show us a right triangle with integers for all sides,
-- whose sides are all less than or equal to 10, and whose permieter is 24
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
