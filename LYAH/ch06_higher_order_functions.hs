{--

    Notes on Learn You a Haskell Chapter 6: Higher Order Functions

    http://learnyouahaskell.com/higher-order-functions

--}

-- Higher order functions can take functions as parameteres and return functions
-- as return values.

{---------------------}
{- CURRIED FUNCTIONS -}
{---------------------}

-- Functions in Haskell officially only accept a single parameter.
-- All functions that take multiple parameters are referred to as
-- "curried functions".

{- Example:

    max 4 5 --> this effectively returns a function that takes a parameter
                and returns either 4, or the parameter. 5 is then applied
                to the function, and since 5 > 4, 5 is returned.

    Putting a space between the two inputs is called "function application"

    Example below...

-}

returnAtLeast4 = max 4 -- returnAtLeast4 will now return either 4 or a greater value

-- If we call a function with too few parameters, we get back a partially applied
-- function, which is a function that takes as many parameters as we left out.
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Now lets make a few partially applied functions
multTwoWithNine = multThree 9 -- will expect two additional parameters
multWithEighteen = multTwoWithNine 2 -- will expect one additional parameter

-- Consider the following two functions. What if we wanted to create a function
-- that takes a number and compares it to 100?  Here's one way:
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- But we could also do this:
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

-- Infix functions can be partially applied using sections, although they
-- must be surrounded with parentheses.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Here's a function that checks if a character supplied to it is an uppercase letter:
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

{-------------------}
{- HIGHER ORDERISM -}
{-------------------}

-- Functions c an take functions as parameters and also return functions.
-- Here's a function that takes a function and applies it twice to something:
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Try something like applyTwice (+3) 10

-- Here's a function that takes a function and two lists as parameters, and then
-- joins the two lists by applying the funciton between the corresponding elements:
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Here's another function which takes a function and returns it with the arguments flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- But this function can also be written like this:
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

{--------------------}
{- MAPS AND FILTERS -}
{--------------------}

-- Keep in mind that map takes a function and a list and applies that function
-- to every element in that list.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Filter takes a function that returns a Bool and a list and applies that function
-- to the list, returning a list where Bool evaluated to true.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- We can rewruite quicksort using filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- Let's find the largest number under 100,000 that is divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Now let's find the sum of all odd squares smaller than some value
sumOddSquaresUnder x = sum (takeWhile (<x) (filter odd (map (^2) [1..])))

-- We can also write this with list comprehension
sumOddSquaresUnder' x = sum (takeWhile (<x) [n^2 | n <- [1..], odd (n^2)])
