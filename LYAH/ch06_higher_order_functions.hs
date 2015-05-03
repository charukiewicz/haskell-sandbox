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

-- Let's look at Collatz sequences. Take a natural number.  If that number
-- is even, divide it by two. If it is odd, multiply by three and add one.
-- We take the resulting number and apply the same thing to it, which
-- produces a new number and so on. The output is a chain of numbers.

-- Let's answer the following question: for all starting numbers between 1
-- and 100, how many chains have a length greater than 15? First, we write
-- a function that produces a chain:
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- Now we write the function that answers the question
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- Here's a funny example. Let's map (*) to a list of numbers. This will
-- return a list of functions.
listOfFuns = map (*) [0..]

{-----------}
{- LAMBDAS -}
{-----------}

-- Lambdas are anonymous functions that are used because we need certain functions
-- only once. Typically, we make a lambda with the sole purpose of passing it to a
-- higher-order funciton. Lambdas begin with the \ character, followed by params,
-- followed by a ->, followed by a function body. Usually wrapped in parentheses.

-- Here's an example.  We will rewrite numLongChains (above) using a lambda instead
-- of a where expression, which we used to declare the isLong function for a single use
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- Lambdas are expressions. The lambda expression above returns a function that tells
-- us whether the length of the list passed to it is greater than 15.

{- 
    Many people make the mistake of using lambdas when partial function application
    does the job more effectively.

    Example: map (+3) [1,6,3,2]
    vs.      map (\x -> x + 3) [1,6,3,2]

    Clearly, the partial function application (first line) is more readable.
-}

-- Lambdas can take any number of parameters
someZip = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- You can match patterns in Lambdas as well
addMyPairs = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- Lambdas are normally surrounded by parentheses, unless we mean for them to
-- extend all the way to the right.  The following two are equivalent:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

{---------}
{- FOLDS -}
{---------}

-- Folds are like the map function, but they reduce the list to a single value.

-- A fold takes a binary function (a function that takes two arguments), a starting
-- value (the accumulator), and a list to fold up. The binary function is called with the
-- accumulator and the first (or last) element and produces a new accumulator.
-- Then the binary function is called again with the new accumulator and new list.

-- Here is an application of the foldl function, which folds a list from the left side.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

{-
    Breakdown of the above:

        - \acc x -> acc + x     the binary function
        - 0                     the starting value
        - xs                    the list to be folding up

-}

-- We can write this implementation even more succinctly:
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- The lambda function (\acc x -> acc + x) is the same as (+). We leave out the xs
-- as the parameter because calling foldl (+) 0 returns a function that takes a list.
-- Generally, you can replace something like (foo a = bar b a) with (foo = bar b)
-- thanks to currying.

-- Lets implement the elem function (checks whether a value is part of a list)
-- using left fold.
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- Right fold, foldr, works in a similar way to left fold, except that the accumulator
-- eats up values from the right. Also, the left fold's binary function has the
-- accumulator as the first parameter and the current value as the second (\acc x -> ...),
-- while the right fold's binary function has the current value as the first parrameter
-- and the accumulator as the second one (\x acc -> ...)

-- We will implement the map function with foldr. The accumulator will be a list, and
-- we'll be accumulating the mapped list element by element. The starting element will be
-- an empty list.
mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

-- We could do this with a fold left as well
mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- Folds can be used to implement any function where you traverse a list once,
-- element by element, and then return something based on that.  Whenever you
-- want to traverse a list to return something, chances are you want a fold.

-- foldl1 and foldr1 work much like foldl and foldr, but you do not need to
-- provide them with a starting value.  They assume the first (or last) element
-- of the list to be the starting value and then start the fold with the element
-- next to it. sum can be implemented as (sum = foldl1 (+)) but this will cause
-- a runtime error if called on an empty list.

-- Here's a bunch of standard library functions implemented with folds:
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scanl and scanr are like foldl and foldr, except they report all the intermediate
-- accumulator states in the form of a list. scanl1 and scanr1 exist as well

-- How many elements does it take for the sum of the roots of all natural numbers
-- to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

{-------------------------------}
{- FUNCTION APPLICATION WITH $ -}
{-------------------------------}

-- $ is called "function application"
-- When a $ is encountered, the expression on the right is applied as the
-- parameter to the function on the left.

{-
    For example:        sum (map sqrt [1..130])
    can be written as   sum $ map sqrt [1..130]

    Or:                 sqrt (3 + 4 + 9)
    can be written as   sqrt $ 3 + 4 + 9

-}

sqrtOfSumOfThree :: (Floating a) => a -> a -> a -> a
sqrtOfSumOfThree x y z = sqrt (x + y + z)

sqrtOfSumOfThree' :: (Floating a) => a -> a -> a -> a
sqrtOfSumOfThree' x y z = sqrt $ x + y + z

{------------------------}
{- FUNCTION COMPOSITION -}
{------------------------}

-- The . operator is used for function composition.
-- One of the uses for function composition is making functions on the fly
-- to pass to other functions. Many times, function composition is cleaner
-- and more concise than using lambdas.

-- Say we have a list of numbers and we want to turn them all into negative numbers.
-- Here's one way to do this:
negateList :: (Num a) => [a] -> [a]
negateList l = map (\x -> negate (abs x)) l

-- Here's another way do do the same thing:
negateList' :: (Num a) => [a] -> [a]
negateList' l = map (negate . abs) l

-- Sometimes, function composition is necessary to create a partially applied function
someRandomFn x = ceiling (negate (tan (cos (max 50 x))))

-- We want to applying currying and remove x on both sides, but we can't.
-- Taking (cos (max 50)) would not make any sense. You cannot take the cos of a function
-- But we can express this function as a composition of functions
someRandomFn' = ceiling . negate . tan . cos . max 50

-- This is called a point free style, which is oftentimes more readable and concise.
-- But long chains of composition are discouraged, as the complexity of a function
-- can increase rapidly. Using let bindings to give labels to intermediary result
-- or split the problem into sub-problems is preferred over a huge composition chain.

-- Here are three examples of function we wrote earlier, which found the sum of
-- all odd squares smaller than 10,000.

-- Here's what we wrote before:
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Here it is with function composition
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Here it is with let bindings
oddSquareSum'' :: Integer
oddSquareSum'' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
