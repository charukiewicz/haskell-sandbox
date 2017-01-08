module Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs n = if n >= 0 then n else (-1)*n

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

x = (+)

fn xs = w `x` 1
    where w = length xs

fst' (a,b) = a
