module Trivial where

data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False

data Date =
    Date DayOfWeek Int

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
      weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a =
    Identity a

-- The `Eq a` part of our instance declaration is there because
-- we do not really know anything about the `a` type, so we must
-- rely on it having an Eq instance
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

-- Exercises:

data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn int1) (TisAn int2) = int1 == int2

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i1 i2) (Two j1 j2) = i1 == j1 && i2 == j2

data StringOrInt =
      TisAnInt   Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
    (==) (TisAString s1) (TisAString s2) = s1 == s2
    (==) _ _ = False

data EitherOr a b =
      Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a1) (Hello a2) = a1 == a2
    (==) (Goodbye b1) (Goodbye b2) = b1 == b2
    (==) _ _ = False
