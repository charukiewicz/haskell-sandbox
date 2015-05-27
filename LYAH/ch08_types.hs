{-

    Notes on Chapter 8 of Learn You a Haskell

    Making Our Own Types and Typeclasses

    http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-}

{------------------------}
{- ALGEBRAIC DATA TYPES -}
{------------------------}

-- So far we have run into a bunch of data types.  Time to make our own.

-- This is how Bool is defined in the standard library
data Bool' = False | True

{- Breakdown:

    data            ->  we're defining a new data type
    Bool'           ->  the name of the new type
    False, True     ->  value constructors (different values this type can have)
    |               ->  or

    So, this can be read as:

        "the Bool' type can have a value of True or False"

-}

-- Now let's make a type that can represent shapes
-- The Shape data type will take Circle and Rectangle value constructors
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- The Shape type dervies the Show typeclass

-- Value constructors (Circle, Rectangle) have fields (Floats) which are
-- the parameters of the value constructurs.

-- Let's make a function that takes a shape and returns its surface
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Notice that the type is Shape -> Float, and not something like Circle -> Float.
-- Doing the latter is impossible, because Circle is not a type, Shape is. We
-- would not be able to write a function of type True -> Int, for example.

-- Notice that we can pattern match against constructors.

-- If we try to print out Circle 10 20 5 in the prompt, we get an error.
-- Haskell does not know how to display our data type as a string; Haskell first
-- runs the show function to get the string representation of our value.

-- So to make the Shape type part of the Show typeclas, we do the following:
-- data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float deriving (Show)

-- This is okay, but we can make it better.
data Point' = Point' Float Float deriving (Show)
data Shape' = Circle' Point' Float | Rectangle' Point' Point' deriving (Show)

-- Note that when defining a point, we used the same name for the data type and
-- the value constructors. This has no special meaning, but frequently used when
-- there is only one value constructor.

-- Here is our new surface function
surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point' x1 y1) (Point' x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Now let's make a function that nudges a shape.  Takes a shape, the amount to
-- move it on the x axis, the amount to move it on the y axis, and then returns
-- a new shape that has the same dimensions, located elsewhere.
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point' x y) r) a b = Circle' (Point' (x+a) (y+b)) r
nudge (Rectangle' (Point' x1 y1) (Point' x2 y2)) a b = Rectangle' (Point' (x1+a) (y1+b)) (Point' (x2+a) (y2+b))

-- Now lets make some functions that allow us to skip having to deal with points
-- These functions will create sahpes at the zero coords and then nudge those
baseCircle :: Float -> Shape'
baseCircle r = Circle' (Point' 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width height = Rectangle' (Point' 0 0) (Point' width height)


{-

We can export our data types in modules.  To export all of our functions
and types so far, we would do the following

module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

Using Shape(..), we export all of the value constructors for Shape. This is the
same as writing Shape (Rectangle, Circle)

We can also opt not to export any value constructors by just writing Shape.
This would allow someone importing our module to only make shapes via the auxilliary
functions (baseCircle, baseRect)

-}

{-----------------}
{- RECORD SYNTAX -}
{-----------------}
