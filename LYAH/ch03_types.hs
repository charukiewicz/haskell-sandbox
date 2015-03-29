{-

    Notes from Chapter 3 of Learn You a Haskell for Great Good

    http://learnyouahaskell.com/types-and-typeclasses

-}

{---------}
{- TYPES -}
{---------}

-- We can use :t in GHCi to examine the type of an expression or a function
-- Haskell has an intricate type system that evaluates the type of every
-- expression at compile time.  This eliminates many runtime errors ahead
-- of time and leads to "safer" code.

-- Haskell also uses type inference.  Types do not have to (but can, and often
-- should) be explicitly defined.

a = 3       -- a :: Integer
b = 'a'     -- b :: Char
c = (1,2)   -- c :: (Integer, Integer)
d = False   -- d :: Bool

-- When writing functions, we can choose to give them an explicit type declaration.

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

{- COMMON TYPES

    Int     - used for whole numbers; integers.  Bounded on 32-bit machines to a max of 2147483647
    Integer - also an integer... except it can be MUCH bigger than an Int
    Float   - floating point decimal with single precision
    Double  - floating point decimal with double precision
    Bool    - boolean type.  Can only have two values: True or False
    Char    - represents a character.  Denoted by single qutes.  A list of characters is a string

    Note that types are always written with a leading capital case.
-}

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

{- TYPE VARIABLES

    Consider the type of the head function (which returns the first element of a list
    
    head :: [a] -> a

    In this case, a is not a type (not a capital case), is is a type variable. This means
    that a can be of any type.  Functions with type variables are called polymorphic functions.

    Another example, the fst function (returns the first element in a pair):

    fst :: (a,b) -> a

    In this tells us that a and b do not have to be different types. But it does tell us that
    the first component's type and the return value's type are the same.

-}

{----------------}
{- TYPE CLASSES -}
{----------------}

-- A typeclass is a sort of interface that defined some behavior. Consider the type signature of ==
--
-- (==) :: (Eq a) => a -> a -> Bool
--
-- In this case, everything before the => symbol is called the class constraint.  This type declaration
-- can be read as: "the equality function takes any two values that are the same type and returns a Bool.
-- The type of those two values must be a member of the Eq class."
--
-- The Eq typeclass provides an interface for testing equality. Any type which makes sense to test for equality
-- should be a member of the Eq class.

{- COMMON TYPECLASSES

    Eq      - supports equality testing. Implemented by: ==, /=
    Ord     - for types that have an ordering. Implemented by: >, <, >=, <=
    Show    - for types that can be presented as strings.  Implemented by: show (ex: show 3 returns "3")
    Read    - the opposite of Show.  Implemented by: read (ex: read "5" - 2 returns 3)
    Emum    - supports sequentially ordered types. Used with the following types: (), Bool, Char, Ordering, etc
    Bounded - members have an upper and lower bound. Implemented by: minBound, maxBound
    Num     - Numeric typeclass. Its members have the ability to act like numbers.
    Integral - Numeric typeclass. Like Num, but only supports whole numbers. Types: Int, Integer
    Floating - Numeric typeclass. Like Num, but only supports floating point numbers. Types: Float, Double

-}
