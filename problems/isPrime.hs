isPrime :: Integer -> Bool
isPrime n = isPrimeAux n 2
    where isPrimeAux n k
            | n == 1 = False
            | (k^2) > n = True
            | n `mod` k == 0 = False
            | otherwise = isPrimeAux n (k + 1)
