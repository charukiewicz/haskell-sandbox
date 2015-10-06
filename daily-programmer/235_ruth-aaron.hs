-- Auxiliary function to calculate prime-ness
-- Recursively checks modulus of all numbers up to input
isPrimeAux :: Int -> Int -> Bool
isPrimeAux x n
    | x == 1 = False
    | x == n = True
    | otherwise = (x `mod` n /= 0) && isPrimeAux x (n + 1)

-- Wrapper function, passes input value to isPrimeAux
isPrime :: Int -> Bool
isPrime x = isPrimeAux x 2

-- Creates list of prime factors of a given input number
primeFactors :: Int -> [Int]
primeFactors n = [ x | x <- [1..n], isPrime x, n `mod` x == 0]

-- Given tuple of integers, returns whether they are ruth-aaron numbers or not
checkRuthAaron :: (Int, Int) -> String
checkRuthAaron (x,y)
    | sum (primeFactors x) == sum (primeFactors y) = "VALID"
    | otherwise                                    = "NOT VALID"
