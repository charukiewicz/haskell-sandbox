fizzBuzz :: Int -> [Char]
fizzBuzz n = case (n `mod` 3, n `mod` 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _      -> show n

--unlines (map fizzBuzz [1..100])

fizzbuzz n = (test 3 "Fizz" . test 5 "Buzz") id (show n)
  where test d s x | n `mod` d == 0 = const (s ++ x "")
                   | otherwise = x
