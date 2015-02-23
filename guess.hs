import Text.Printf

main = do
  putStrLn "Think of a number from 1-100. I'll guess it. Type 'y'/'l'/'h' for yes/lower/higher resp."
  guess 1 100

guess min max 
  | min > max = putStrLn "Uh oh. Let's try again" >> main
  | otherwise = do  
    let m = (min + max) `div` 2
    putStrLn $ printf "Is your number %d?" (m :: Int)
    ans <- getLine
    case ans of
      "y" -> putStrLn "Hurrah. Let's play again." >> main
      "l" -> guess min (m-1)
      "h" -> guess (m+1) max
      _   -> putStrLn "Nope. Let's try that again" >> guess min max
