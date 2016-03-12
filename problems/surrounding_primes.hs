{-

From codewars.com:

We need a function prime_bef_aft() that gives the largest prime below a certain
given value n, befPrime, and the smallest prime larger than this value, aftPrime.

The result should be output in a list like the following:

    PrimeBefAft == (befPrime, aftPrime)

If n is a prime number it will give two primes, n will not be included in the result.

Let's see some cases:

    primeBefAft(100) --> (97, 101)

    primeBefAft(97) --> (89, 101)

    primeBefAft(101) --> (97, 103)


-}

isPrimeAux :: Integer -> Integer -> Bool
isPrimeAux n k
  | k^2 > n = True
  | n `mod` k == 0 = False
  | otherwise = isPrimeAux n (k + 1)

isPrime :: Integer -> Bool
isPrime n = isPrimeAux n 2

greatestPrimeBelow :: Integer -> Integer
greatestPrimeBelow n
  | isPrime n = n
  | otherwise = greatestPrimeBelow (n - 1)

lowestPrimeAbove :: Integer -> Integer
lowestPrimeAbove n
  | isPrime n = n
  | otherwise = lowestPrimeAbove (n + 1)

primeBefAft :: Integer -> (Integer, Integer)
primeBefAft n = (greatestPrimeBelow (n - 1), lowestPrimeAbove (n + 1))
