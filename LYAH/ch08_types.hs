{-

    Notes on Chapter 8 of Learn You a Haskell

    Making Our Own Types and Typeclasses

    http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-}

import qualified Data.Map as M

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

-- We've been tasked with creating a data type that describes a person.
-- The data we want to store is: first, last, age, height, phone, and favorite
-- ice cream flavor.
data Person' = Person' String String Int Float String String deriving (Show)

guy = Person' "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- Now, if we wanted to create functions to get separate info from a person,
-- we would have to do something like this
firstName' :: Person' -> String
firstName' (Person' firstname _ _ _ _ _) = firstname

lastName' :: Person' -> String
lastName' (Person' _ lastname _ _ _ _) = lastname

age' :: Person' -> Int
age' (Person' _ _ age _ _ _) = age

height' :: Person' -> Float
height' (Person' _ _ _ height _ _) = height

phoneNumber' :: Person' -> String
phoneNumber' (Person' _ _ _ _ number _) = number

flavor' :: Person' -> String
flavor' (Person' _ _ _ _ _ flavor) = flavor

-- This is a rather tedious way of extracting data we need.  Fortunately, there's
-- a better way.  We use Record Syntax.
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Eq, Show, Read)

-- Using this syntax creates the same data type that we created earlier, but
-- it also creates the lookup functions for each field (firstName, age, etc...)

-- The other benefit to using record syntax is different data display so long as
-- we use record syntax to define and instantiate the type.  Say we have a car
-- and we want to keep track of the company that made it, the model, and the  year.
-- Non-record syntax:
data Car' = Car' String String Int deriving (Show)

car1 = Car' "Ford" "Mustang" 1967

-- Record Syntax:
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

car2 = Car {company="Ford", model="Mustang", year=1967}

-- Record syntax is useful when it is not obvious which field is which. For example,
-- a 3D vector data type can be defined as data Vector = Vector Int Int Int
-- It's obvious that the fields are the components of the vector, but in Person and
-- in Car, it was not so obvious which field was which.

{-------------------}
{- TYPE PARAMETERS -}
{-------------------}

-- We can pass in type parameters when declaring a data type. Sometimes this is
-- not very useful.  Instead of declaring Car the way we did above, we could do
data Car'' a b c = Car'' { company' :: a
                         , model' :: b
                         , year' :: c
                         } deriving (Show)

-- But there is not any point in doing so in this case. And the downside is that
-- now any function that uses our new Car'' data type would have a more complex
-- function definition.
tellCar'' :: (Show a) => Car'' String String a -> String
tellCar'' (Car'' {company' = c, model' = m, year' = y}) = "Company: " ++ c ++ " Model: " ++ m ++ " Year: " ++ show y

-- Since we would usually end up using Car String String in most of the time,
-- paramaterizing the Car type is not worth it.

-- Let's implement a 3D vector type and add some operations for it. We'll use a
-- parameterized type because although it'll usually contain numeric types, it
-- will still support several of them.
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMulti :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMulti` m = Vector (i*m) (j*m) (k*m)

scalarMulti :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMulti` (Vector l m n) = i*l + j*m + k*n

-- Notice that we do not put Num class constraint in the data declaration, because
-- we would have to repeat it in the functions anyway.  It is important to
-- distinguish between the type constructor and the value constructor. The type
-- constructor before the = is the type constructor, and the constructors after it
-- (separated by |'s) are value constructors.

{---------------------}
{- DERIVED INSTANCES -}
{---------------------}

-- In imperative languages like Java, Python, C++, classes are a blueprint from
-- which we then create objects that contain state and can do some actions.
-- Typeclasses are more like interfaces. We don't make data from typeclasses.
-- Instead, we make our first data type and then we think about what it can act like.
-- If it can act like something that can be equated, we make it an instance of the
-- Eq typeclass. If it can be ordered, we make it an instance of the Ord typeclass.

-- We declared a Person data type above.  Currently it only derives Show.  It
-- makes sense for us to compare whether or not two people are identical, so we 
-- can give it the Eq typeclass.

{-
        data Person = Person { firstName :: String  
                             , lastName :: String  
                             , age :: Int  
                             } deriving (Eq)  
-}

-- We also have the Show and Read typeclasses to accomodate conversion to or
-- from strings.

{-
        data Person = Person { firstName :: String  
                             , lastName :: String  
                             , age :: Int  
                             } deriving (Eq, Show, Read)  
-}

-- We can also easily use algebraic data types to make enumerations and the
-- Enum and Bounded typeclasses help us with that.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Now we can use operators like == or > or `compare` on days of the week.
-- We can also use functions like minBound, maxBound, succ, and pred

{-----------------}
{- TYPE SYNONYMS -}
{-----------------}

-- Previously we mentioned that [Char] and String types are equivalent and
-- interchangeable. This is implemented with type synonyms.

-- This interchangability in the standard library is defined as:
--      type String = [Char]

-- The type keyword is not actually making anything new, just declaring a synonym
-- for an existing type.

-- Let's look at the phonebook from much earlier
phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

-- We can see that the type of phoneBook is [(String,String)].  Let's declare a
-- type synonym representing this
type PhoneBook' = [(String,String)]

-- And now lets make a synonym for String as well
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- We can now implement a function that uses these new types
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- We can also parameterize types
type AssocList k v = [(k,v)]

-- Or we can partially apply them like this:
type IntMap v = M.Map Int v

-- Or we can curry them
type IntMap' = M.Map Int

-- We can use the Either data type to represent one of two options for data
-- This can be more informative than using the Maybe data type, as its possibility
-- of Nothing might not be very helpful

-- Let's say we have lockers in a high school. Lockers have codes and they may
-- already be taken by someone else.
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

-- We make a type synonym for the type that maps from integers to pairs of
-- locker state and code. Now we're going to make a function that searches
-- for the code in a locker map. We use Either String Code to represent the
-- result. This is because the lookup can fail in two ways, the locker can
-- be taken, or the locker might not exist at all.
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case M.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = M.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

{-----------------------------}
{- RECURSIVE DATA STRUCTURES -}
{-----------------------------}
