module TakeDrop where

take16 x = take 16 x

drop4take1 x = take 1 $ drop 4 x


drop9 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs x = (take 7 $ drop 9 x) ++ (take 4 $ drop 5 x) ++ (take 5 x)
