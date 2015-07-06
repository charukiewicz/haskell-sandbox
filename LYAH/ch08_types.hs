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
data Bool' = False' | True'

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
surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

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
surface' (Rectangle' (Point' x1 y1) (Point' x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- Now let's make a function that nudges a shape.  Takes a shape, the amount to
-- move it on the x axis, the amount to move it on the y axis, and then returns
-- a new shape that has the same dimensions, located elsewhere.
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point' x y) r) a b = Circle' (Point' (x+a) (y+b)) r
nudge (Rectangle' (Point' x1 y1) (Point' x2 y2)) a b = Rectangle' (Point' (x1+a) (y1+b)) (Point' (x2+a) (y2+b))

-- Now lets make some functions that allow us to skip having to deal with points
-- These functions will create sahpes at the zero coords and then nudge those
baseCircle :: Float -> Shape'
baseCircle = Circle' (Point' 0 0)

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

-- We can create recursive data types, where one value of some type contains
-- values of that type, which in turn contain more values of the same type.

-- Let's use ADTs to make our own list
data List'' a = Empty'' | Cons'' a (List'' a) deriving (Show, Read, Eq, Ord)

-- Here's another version:
data List' a = Empty' | Cons' { listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

-- We can definefunctinos to be automatically infix by making them comprised of
-- only special characters.  We can also do the same with constructurs, since
-- they're just functions that return a data type
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- Here we see the fixity declaration preceding the data type.  When we define
-- functions as operators, we can use this to give them a fixity. A fixity states
-- how tightly the operator binds and whether it's left-associative or
-- right-associative.  (We would use infixl for left associativity)

-- Let's make a function that adds two of our lists together
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Let's implement a binary search tree data type
-- A tree is either an empty ree or it's an element that contains some value
-- and two trees.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Now we will make two functions so that we do not have to manually build a
-- Tree. The first is a utility function that makes a single node tree. The
-- second allows us to insert an element into the tree.
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- Now let's make a function that allows us to check whether an element is in a
-- specified Tree.
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- Now let's play with these functions
nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

{-------------------}
{- TYPECLASSES 102 -}
{-------------------}

-- Typeclasses are like interfaces for data types. A typeclass defines some
-- behavior (eg. comparing for equality or ordering). Types that we want to
-- behave this way are made instances of that typeclass.

-- For example, the Eq typeclass is for stuff that can be equated. It defines
-- the functions == and /=. If we have a type (say, Car) and comparing two cars
-- with the equality function == makes sense, then it makes sense for Car to be
-- an instance of Eq.

-- This is how Eq is defines in Prelude
class Eq' a where
    (.==) :: a -> a -> Bool
    (./=) :: a -> a -> Bool
    x .== y = not (x ./= y)
    x ./= y = not (x .== y)

-- We implemented the function bodies for the functions that Eq defines in terms
-- of mutual recursion.

-- The following data type defines the states of a traffic light. We won't derive
-- any class instances for it; we're going to write up some instances by hand.
data TrafficLight = Red | Yellow | Green

-- Now here's how we make it an instance of Eq
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- We used the "instance" keyword
-- "class" is for defining new typeclasses
-- "instance" is for making our types instances of typeclasses

-- When we defined Eq, we used 'a' as the type variable to play the role of
-- whichever type would be made an instance later on.  When defining the instance,
-- we replaced 'a' with the actual type.

-- Because in the Eq typeclass, == was defined in terms of /= and vise versa,
-- we only had to overwrite one of them in the instance declaration. This is
-- called a minimal complete definition for the typeclass.

-- If the definition of the Eq typeclass only included the first two lines, we
-- would have to define BOTH == and /= in our instance, which would be significantly
-- longer.

-- Now let's make this data type an instance of Show by hand, as well.
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- Typeclasses can also be subclasses of other typeclasses.
-- The class declaration for the Num typeclass starts with
--
--      class (Eq a) => Num a where
--
-- This essentially says that a type must be an instance of Eq before we can
-- make it an instance of Num.

-- Now, how do we apply typeclasses to non-concrete types?  For example, we cannot
-- use Maybe as a concrete type (like Char or Int).  Maybe is a type constructor
-- that takes one parameter and then produces a concrete type.
data Maybe' a = Nothing' | Just' a

-- So we can do:
instance (Eq m) => Eq (Maybe' m) where
    Just' x == Just' y = x == y
    Nothing' == Nothing' = True
    _ == _ = False

-- Note the class constraint on this instance declaration. We are saying this:
-- we want all types of the form Maybe m to be part of the Eq typeclass, but
-- only those types where m (the contents of Maybe) is also a part of Eq.

-- Must of the time, class constraints in class declarations are used for making
-- a typeclass a subclass of another typeclass. Class constraints in instance
-- declarations are used to express requirements about the contents of some type.
-- Here, we required the contents of Maybe to also be part of the Eq typeclass.

{----------------------}
{- A YES-NO TYPECLASS -}
{----------------------}

-- In Javascript, 0, "", and false will evalulate to False. But any nonempty
-- string will evaluate to True.  Let's try to implement this type of behavior.

-- We start with a class declaration.
class YesNo a where
    yesno :: a -> Bool

-- This defines one function that takes a value considered to hold some concept
-- of true-ness and gives us a concrete truth value (Bool) in return.

-- Now, to define some instances. For numbers, we assume that any value that isn't
-- 0 is true-ish and that 0 is false-ish.
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

-- Empty lists (and by extension, strings) are a no-ish value, while non-empty lists
-- are a yes-ish value.
instance YesNo [a] where
    yesno [] = False
    yesno _ = True

-- Bool itself also holds true-ness and false-ness
instance YesNo Bool where
    yesno = id

-- the 'id' in this instance is a standard library function that takes a paramter
-- and returns the same thing, which is what we would be writing here anyway

-- Let's make 'Maybe a' and instance as well.
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- An empty Tree (defined earlier) is false-ish and anything that isn't empty
-- is true-ish.
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

-- With our TrafficLight, we can consider Red to be false-ish and anything else
-- to be true-ish.
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- Now let's make a function that handles an if statement with YesNO values
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

{-------------------------}
{- THE FUNCTOR TYPECLASS -}
{-------------------------}

-- So far we have seen a number of typeclasses in the standard library.  We have
-- seen Ord, Eq, Show, Read. Now we are going to look at the Functor typeclass,
-- which is for things that can be mapped over.

-- Let's look at the implementation
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- We can see that the fmap function type definition is interesting in that
-- it contains type variables that are not concrete types. The 'f' is not a
-- concrete type (a type a value can hold, like Int, Bool, or Maybe String),
-- but a type constructor that takes one parameter.
--
-- Recap: 'Maybe Int' is a conrete type, but Maybe is a type constructor that
-- takes one type as the parameter.

-- We see that fmap takes a function from one type to another and a functor
-- applied with one type and returns a functor applied with another type.
--
-- This is similar to the type signature of map, 'map :: (a -> b) -> [a] -> [b]'
-- We see that 'map' is just 'fmap' that works only on lists.

-- Here's how the list is an instance of the Functor typeclass.
instance Main.Functor [] where
    fmap = map

-- Now let's implement the Maybe instance of Functor.
instance Main.Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- Notice how our type variable is 'Maybe' and not 'Maybe m'. We did not want a
-- concrete type (as opposed to our YesNo typeclass, where we did).

-- Now let's implement the Tree instance of Functor.
instance Main.Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (Main.fmap f leftsub) (Main.fmap f rightsub)

-- Now let's implement the Either instance. Note that the Functor typeclass
-- wants a type constructor that takes only one type parameter, but Either
-- takes two.  So we will partially apply Either.
instance Main.Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

-- To wrap up, we have seen that the Functor typeclass enables mapping over a
-- particular type. A quick way of recognizing a type that can be mapped over
-- is thinking whether or not the type is a container. List, Maybe, Tree, etc.

{---------------------------}
{- KINDS AND SOME TYPE-FOO -}
{---------------------------}

-- Type constructors take other types as parameters to eventually produce concrete
-- types, just like how functions take values as parameteres to produce values.
-- Type constructors can be partially applied just like functions can.

-- In this section, we'll formally define how types are applied to type constructors.

-- Types are little labels that values carry so that we can reason about the values.
-- But types also have their own little labels, called kinds.  A kind is somewhat
-- like a type of a type.

-- We can use the :k command in GHCi to examine the kind of a type.

-- :k Int
-- Int :: *             - A '*' represents a concrete type.

-- :k Maybe
-- Maybe :: * -> *      - The maybe type constructors takes one concrete type (Int)
--                        and returns a concrete type (Mabye Int)

-- :k Maybe Int
-- Maybe Int :: *       - The concrete type output.

-- :k Either
-- Either :: * -> * -> *    - Either takes two concrete types and produces a concrete type.

-- To make Either part of the Functor typeclass, we had to partially apply it
-- because Functor wants types of kind '* -> *' and Either is '* -> * -> *'


-- Now, consider the following typeclass
class Tofu t where
    tofu :: j a -> t a j

-- How would we make a type that could be an instance of the Tofu typeclass?
--
-- Since 'j a' is used as the type of value that the tofu function takes as a
-- parameter, it has to have a kind of *.
--
-- We assume * for 'a' and so we can infer that 'j' has to have a kind of * -> *
--
-- We see that 't' has to produce a concrete value and it takes two types.
--
-- Knowing that 'a' has kind of * and 'j' has kind of * -> *, we infer that 't'
-- has to have a kind of * -> (* -> *) -> *
--
-- So it takes a concrete type 'a', a type constructor that takes one concrete
-- type 'j' and produces a concrete type.

-- Now let's make a type with kind of * -> (* -> *) -> *
data Frank a b = Frank {frankField :: b a} deriving (Show)
