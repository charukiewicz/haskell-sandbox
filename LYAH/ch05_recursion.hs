{-
    Learn You a Haskell - Chapter 05: Recursion

-}

-- Recursion is important to Haskell because unlike imperative languages, you
-- do computations in Haskell by declaring what something is instead of how you
-- get it.

-- Lets look at a recursive implementation of the maximum function
-- Returns the maximum element in a list
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Here's a clearer way to write the same thing
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum' xs)

-- Here is a recursive implementation of the replicate function
-- Takes a number and an element and returns a list of that element of that length
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- Here is an implementation of the take function
-- Takes the first specified number of elements from an input list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x :take' (n-1) xs

-- Here is an implementation of the reverse function
-- Reverses a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Here is an implementation of the repeat function
-- Takes an element and returns an infinite list that has just that element
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- Here is an implementation of the zip function
-- Takes two lists and attempts to mash them together, returns a list of tuples
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- Here is an implementation of the elem function
-- Takes an element and a list and returns whether or not the element is in the list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

-- Here's a quicksort implementation.  The posterchild for Haskell.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

