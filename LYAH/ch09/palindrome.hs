main = interact respondPalindromes

{-

respondPalindromes contents = unlines (map (\xs ->
 if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where isPalindrome xs = xs == reverse xs

-}

-- here's the same function in point free style
respondPalindromes = unlines . map (\xs ->
 if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs
