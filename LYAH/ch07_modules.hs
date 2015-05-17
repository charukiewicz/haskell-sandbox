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
import Data.Char -- has a bunch of useful functions for working with characters

-- Now, because the Data.Map module exports functions that clash with Prelude
-- and Data.List, we do a qualified import
import qualified Data.Map as Map -- has a bunch of useful functions related to mapping
-- Same for Data.Set
import qualified Data.Set as Set

-- Now lets import our own custom module
-- import Geometry

-- Now lets import our Geometry submodules separately, instead of the above
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

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

-- Lets use a fold to implement searching a list for a sublist
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

--     isInfixOf   - searches for a sublist (what we just made above)

--     isPrefixOf/isSuffixOf - search for a sublist at the beginning and end, respectively

--     elem/notElem - check if an element is or isn't inside a list

--     partition    - takes a list and a predicate and returns a pair of lists.
--                    the first list in the result contains all the elements that
--                    satisfy the predicate, the second contains all the ones that don't

--     find         - takes a list and a predicate and returns the first element that
--                    satisfies that predicate.  but it returns the element wrapped in
--                    a Maybe value

--    elemIndex     - returns the index of the specified element in a given list.
--                    if not found, it returns Nothing

--    elemIndices   - like elemIndex, but returns a list of indices.

--    findIndex     - like find, but it maybe returns the index of the first element
--                    that satisifes the predicate

--    zip3/zip4...7 - like zip, except takes 3..7 lists

--    zipWith3/zipWith4...7 - like zipWith, except take 3..7 lists

--    lines         - takes a string and returns every line of that string in a
--                    separate list

--    unlines       - the inverse function of lines

--    words/unwords - used to split a line of text into words or joining a list
--                    of words into text

--    nub           - takes a list and removes duplicate elements

--    delete        - takes an element and a list and deletes the first occurence
--                    of the element in the list

--    \\            - the difference function. acts like a set difference, basically
--                    for every element in the right-hand list, it removes a matching
--                    element in the left one

--    union         - acts like a function on sets. returns the untion of two lists

--    intersect     - works like set intersection. returns elements found in both lists

--    insert        - takes an element and a list of elements that can be sorted
--                    and inserts it into the last ordered position

-- The following functions are generic versions (use the Num typeclass instead of the
-- Int typeclass) of their older counterparts:
--
--  length      -> genericLength
--  take        -> genericTake
--  drop        -> genericDrop
--  splitAt     -> genericSplitAt
--  !!          -> genericIndex
--  replicate   -> genericReplicate

-- In the following functions, the first set use == to test for equality, whereas
-- the By ones also take an equality function (i.e. group <-> groupBy (==) )
--  nub         -> nubBy
--  delete      -> deleteBy
--  union       -> unionBy
--  intersect   -> intersectBy
--  group       -> groupBy

-- The following functions also have their more general equivalents.  They take
-- a function that determines if one element is GT, LT, or EQ to the other.
--  sort        -> sortBy
--  insert      -> insertBy
--  maximum     -> maximumBy
--  minimum     -> minimumBy

{-------------}
{- Data.Char -}
{-------------}

-- The Data.Char module exports functions that deal with characters. This is
-- helpful when filtering and mapping over strings since they are lists of Chars.

-- Data.Char exports a bunch of predicates over characters; functions that take
-- a character and tell uis whether some assumption about it is true/false.

--      isControl       - checks whther the character is a control character

--      isSpace         - checks whther a character is a white-space character
--                        (includes spaces, tabs, newlines, etc.)

--      isLower         - checks whether a character is lower-cased

--      isUpper         - checks whether a character is upper cased

--      isAlpha         - checks whether the character is a letter

--      isAlphaNum      - checks whether a character is a letter or a number

--      isPrint         - checks whether a character is printable

--      isDigit         - checks whether a character is a digit

--      isOctDigit      - checks whether a character is an octal digit

--      isHexDigit      - checks whether a character is a hex digit

--      isLetter        - checks whether a character is a letter

--      isMark          - checks for Unicode mark characters (letters with accents)

--      isNumber        - checks whether a character is numeric

--      isPunctuation   - checks whehter a character is punctuation

--      isSymbol        - checks whether a character is a fancy mathematical symbol

--      isSeparator     - checks for Unicode spaces and separators

--      isAscii         - checks whether a character falls into the first 128 characters
--                        of the Unicode character set

--      isLatin1        - checks whether a character falls into the first 256 characters

--      isAsciiUpper/Lower - checks whether a character is ASCII upper/lower-case


-- These functions can be combined with functions like "all" from the Data.List
-- module, which takes a predicate and a list and returns True if the predicate
-- holds for every element in the list.

--      toUpper         - converts a character to upper-case. spaces, etc. are unchanged

--      toLower         - converts a character to lower-case

--      toTitle         - converts a character to title-case

--      digitToInt      - converts a character to Int

--      intToDigit      - inverse of digitToInt (takes 0..15 and converts to lower case char)

--      ord/chr         - convert characters to their corresponding numbers and vise versa

-- Here is a simple Caesar cipher-like shift function, except it is not limited
-- to only the alphabet.
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

-- Now let's make a decoding function, by shifting the number back the same amount
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

{------------}
{- Data.Map -}
{------------}

-- Association lists (aka dictionaries) are lists that are used to store key-value
-- pairs where the ordering does not matter.  Example below:

phoneBook = 
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]

-- Notice that this is just a list of pairs of strings.  The most common task
-- when dealing with association lists is looking up some value by key. Let's
-- make a function that looks up some value given a key.
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- This function takes a key and a list, filters the list so that only matching
-- keys remain, gets the first key-value that matches, and returns the value.
-- However, if we search for a key that does not exist, we get a runtime error.

-- Let's make a function that uses the Maybe data type to avoid this.
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) = if key == k
                             then Just v
                             else findKey' key xs

-- Now let's implement this with a fold
findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- We just imeplemented the lookup function from Data.List.  The Data.Map module
-- offers association lists that are much faster and also provides a lot of
-- utility functions. We imported Data.Map as a qualified import at the top of
-- this file to avoid clashes with Prelude and Data.List.

--      fromList        - takes an association list (in the form of a list) and
--                        returns a map with thesame associations

-- Because the tree data structures that Data.Map uses internall are much faster,
-- we should always use Data.Map for key-value assocaitions unless we have keys
-- that are not apart of the Ord typeclass.

--      empty           - represents an empty map. no args, returns an empty map

--      insert          - takes a key, value, and a map and returns a new map
--                        that is just like the old one with key,value inserted

-- We can implement our own fromList by using an empty map, insert, and fold:
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

--      null            - checks if a map is empty

--      size            - reports the size of a map

--      singleton       - takes a key,value and creates a map that has exactly
--                        one mapping

--      lookup          - returns Just something if it finds something for the
--                        key, Nothing otherwise

--      member          - takes a key and a map and reports whether the key is
--                        in the map or not

--      map/filter      - work like their list equivalents

--      toList          - the inverse of fromList

--      keys/elems      - return lists of keys and values respectively

--      fromListWith    - acts like fromList, except it doesn't discard duplicates
--                        uses a function supplied to it to decide what to do with them

-- lets say that a girl can have several numbers; we have an association list
-- set up as follows:
phoneBook2 =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]

-- if we use fromList to put this into a map, we will lose numbers. so we do:
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

-- if a duplicate key is found, the function we pass is used to combine the values
-- of those keys into some other value

-- we can also first make the values in the association list singleton lists and
-- then we can use ++ to combine the numbers
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

--      insertWith      - is to insert as fromListWith is to fromList

{------------}
{- Data.Set -}
{------------}

-- The Data.Set module offers us sets.  A kind of cross between lists and maps.
-- All elements in a set are unique, and ordered.  Much faster than dealing with
-- lists.  Most common operations are insert, check for membership, convert to list

-- We import Data.Set as a qualified import at the top of this file

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

set1 = Set.fromList text1
set2 = Set.fromList text2

-- Functions:

--      intersection        - shows which elements are shared by two sets

--      difference          - shows which elements are unique to either set

--      union               - shows the elements that appear in either set

-- Other functions include null, size, member, empty, singleton, insert, delete
-- We can map over sets and filter them as well.

--      isSubsetOf/isPropertSubsetOf - checks if a set is a subset of another

-- Set are often used to weed out duplicates from a list by converting it into
-- a set with fromList and then converting it back to a list with toList. This
-- operation is much faster than the Data.List nub function.

setNub :: (Ord a) => [a] -> [a]
setNub xs = Set.toList $ Set.fromList xs

-- Although setNub is generally faster than nub on big lists, nub preserves the
-- ordering of the list's elements, while setNub does not.

{--------------------------}
{- MAKING OUR OWN MODULES -}
{--------------------------}

-- Lets make our own module called Geometry.hs
-- (Take a look at the Geometry.hs file in this directory)

-- We first imported the Geometry module at the top of this file

-- Now take a look at the Geometry directory.  There you find
-- Sphere.hs, Cuboid.hs, and Cube.hs

-- These are submodules of Geometry and are imported as qualified imports
-- at the top of this file.

bigSphereArea = Sphere.area 1000
bigSphereVolume = Sphere.volume 1000

smallCubeArea = Cube.area 0.7
smallCubeVolume = Cube.volume 0.7
