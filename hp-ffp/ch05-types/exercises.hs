{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

-------------------

i :: a -> a
i x = x

c :: a -> b -> a
c = undefined

c'' :: b -> a -> b
c'' = undefined
