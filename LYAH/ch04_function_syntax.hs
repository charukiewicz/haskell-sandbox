{-

    Notes for Chapter 4 of "Learn You a Haskell for Great Good"

    Syntax in Functions

    http://learnyouahaskell.com/syntax-in-functions

-}

{--------------------}
{- PATTERN MATCHING -}
{--------------------}

-- Pattern matching consists of specifying patterns to which some data should conform
-- and then checking it to see if it does and deconstructing the data according to those
-- patterns.

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- Here's a simple function that adds two vectors.
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- Here's a better function that also adds two vectors
-- but makes use of pattern matching to make the function
-- more easily readable.
addVectors' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Lets make some functions that extract values from triples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Lets combine pattern matching with list comprehensions
someListOfPairs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
addPairs l = [ a + b | (a, b) <- l ]

-- Lets make our own head function (a function that returns
-- the first element in a list)
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- Lets make a function that tells us some of the first elements
-- of a list in English form
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long.  The first two elements are " ++ show x ++ " and " ++ show y

-- Here is a length function that uses pattern matching
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- And here is a sum function that uses pattern matching
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- We can use the @ operator to give a pattern a refernece name
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

{----------}
{- GUARDS -}
{----------}

-- Patterns are a way of making sure a value confirms to some form
-- and deconstructing it. But guards are a way of testing whether
-- some property of a value (or several) are true or false.

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal.  Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat!  Lost some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal.  Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat!  Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"

-- Lets implement our own max function.
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- Now lets implement our own compare
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

{---------}
{- WHERE -}
{---------}

-- We can use where statements to avoid having to write out the same
-- expression over and over again in the guards.  Let's revisit the
-- BMI functions

betterBmiTell :: (RealFloat a) => a -> a -> String
betterBmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal.  Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat!  Lost some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

-- We can make this even better

betterBmiTell' :: (RealFloat a) => a -> a -> String
betterBmiTell' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal.  Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat!  Lost some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0) -- this is actually pattern matching

-- Here's some more pattern matching

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- We can define functions in the where bindings as well

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- Where bindings can also be nested

{-------------------}
{- LET-IN BINDINGS -}
{-------------------}

-- Like where beindings, let bindings let you bind variables in a function.
-- Unlike where bindings, which bind variables at the end of a function, let
-- bindings allow you to bind variables anywhere and are expressions themselves.

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- The difference is that let bindings are expessions themselves, but where
-- bindings are just syntactic constructs. Let bindings can go almost anywhere.
meaningOfLife = 4 * (let a = 9 in a + 1) + 2

-- They can also be used to intruduce functions in a local scope.
squareSomething = let square x = x * x in (square 5, square 3, square 2)

-- To define several variables inline, we use semicolons.
multiVarLet = (let a = 100; b = 200; c = 300 in a*b*c, let foo = "Hey "; bar = "there!" in foo ++ bar)

-- We can also put let bindings inside list comprehensions. Lets rewrite our
-- calcBmis function.
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

{--------------------}
{- CASE EXPRESSIONS -}
{--------------------}

-- Case expressions are very similar to pattern matching. In fact, pattern
-- matching is syntactic sugar for case expressions. Here's an example, the
-- following two functions are interchangable:
myHead :: [a] -> a
myHead [] = error "No head for empty lists!"
myHead (x:_) = x

myHead' :: [a] -> a
myHead' xs = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x

{-
  The general syntax for case expressions is:

    case expression of pattern -> result
                       pattern -> result
                       pattern -> result
                       ...
-}

-- Case expressions can be used pretty much anywhere, as opposed to pattern
-- matching on function parameters, which must be done when defining functions.
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- Since pattern matching in function definitions is syntactic sugar for case
-- expressions, the above function could have been defined as follows:
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
