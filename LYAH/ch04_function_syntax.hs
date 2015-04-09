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
