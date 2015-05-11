{-

    Notes on Learn You a Haskell

    Chapter 7: Modules

    http://learnyouahaskell.com/modules

-}

-- A module in Haskell is a collection of related functions, types, and typeclases.
-- A Haskell program is a collection of modules where the main module loads
-- other modules and then uses the functions defined in them to do something.

-- The Haskell standard library is split into modules. All of the functions we have
-- used so far have been part of the Prelude module, which is imported by default.

-- The syntax for importing modules is `import <module name>`. This must be done
-- before any functions (at the top of the file).


import Data.List -- has a bunch of useful functions for working with lists


-- Let's use a function from the Data.List module (nub) to count the number of
-- unique elements in a list. The nub function weeds out duplicate elements.
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- Modules can be loaded in ghci using the following syntax:
--  :m + <module1 name> <module2 name> ...

-- We can import only a few of the functions from a module using the following syntax
--  import Data.List (nub, sort)

-- We can also import all functions other than a particular function use the following
--  import Data.List hiding (nub)

-- We can deal with clashes in function names by importing modules like this:
--  import qualified Data.Map
--
-- We can then access the functions like this: Data.Map.filter
--
-- We can also rename a module as we import it
--  import qualified Data.Map as M
--
-- And then access functions like this: M.filter

{-------------}
{- Data.List -}
{-------------}

-- The Data.List module provides some very useful functions for dealing with lists.
-- Some of the functions in Data.List (like map and filter) are imported by Prelude
-- by default.

-- Functions

--     intersperse - takes an element and a list and then puts that element in
--                   between each part of elements in the list

--     intercalate - takes a list of lists and a list, inserts that list in between
--                   all of those lists and flattens the result

--     transpose   - transposes a list of lists. in a 2D matrix, the columns become
--                   the rows and vice versa

--     foldl',foldl1' - stricter versions of their respective lazy incarnations
--                      normal folds are lazy, where the accumulator is not actually
--                      updated as the folding happens. these strict folds actually
--                      compute the intermediate values as they perform the folding,
--                      so they avoid stack overflow errors.

--     concat      - flattens a list of lists into just a list of elements

--     concatMap   - is the same thing as first mapping a function to a list and then
--                   concatenating the list with concat

--     and         - takes a list of boolean values and returns True only if all the
--                   values in the list are true

--     or          - like and, except it returns True if any of the bool values in
--                   the list are true

--     any/all     - take a predicate and then check if any or all the elements in a
--                   list satisfy the predicate, respectively

--     iterate     - takes a function and a starting value. it applies the function
--                   to the starting value, then it applies that function to the
--                   result, then it applies it to the result again, etc. returns
--                   all of the results in the form of an infinite list

--     splitAt     - takes a number and a list, splits the list at that element index
--                   returns two lists in a tuple

--     takeWhile   - takes elements from a list while the predicate holds. when an
--                   element is encountered that doesn't satisfy the predicate, it
--                   is cut off

--     dropWhile   - is similar to the above, except it drops all the elements while
--                   the predicate is True, and then it returns the rest of the list

--     span        - similar to takeWhile, except it returns a pair of lists

--     sort        - sorts a list. the elements in the list have to be part of the
--                   Ord typeclass; if they cannot be ordered, they cannot be sorted

--     group       - takes a list and groups adjacent elements into sublists if they
--                   are equal
countElements :: (Ord a) => [a] -> [(a, Int)]
countElements l = map (\l@(x:xs) -> (x,length l)) . group . sort $ l

--     inits/tails - like init and tail, only they apply recurisvely to a list until
--                   there is nothing left

--     isInfixOf   - searches for a sublist
