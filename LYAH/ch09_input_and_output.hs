{-

    Learn You a Haskell - Chapter 9

    Input and Output

    notes

-}

-- In imperative languages, you get things done by giving computers a series of
-- steps to execute. In functional programming, you define what things are.
-- In Haskell, functions produce no side-effects.  A particular input given to
-- a function will always return the exact same output. In imperative languages,
-- you hafve no such guarantee.

-- But sometimes the outside has to be changed.  Haskell has a clever system for
-- dealing with functions that have side-effects that separates the part of our
-- program that is pure from the part that is impure, which does all the work of
-- doing things like talking to the keyboard and the screen.

-- Up until now, we've used GHCi for everything.  Time to write our first _real_
-- Haskell program.

{- >> helloworld.hs <<

    main = putStrLn "hello, world"

-}

-- Looking at the type of putStrLn, we see it is String -> IO ().  The function
-- takes a string and returns an I/O action that has a result of type () (i.e.
-- the empty tuple, also known as the unit).

-- An I/O action is something that, when performed, will carry out an action with
-- a side-effect (usually reading from input or printing stuff to the screen),
-- and will also contain some kind of return value inside it. Printing a string
-- to the terminal does not have a meaningful return value, so a dummy value of
-- () is used.

-- The empty tuple is a value of () and also has a type of ()

-- An I/O action will be performed when we give it a name of main and then run
-- our program.

-- But having the whole program be just one I/O action is limiting. We use do
-- syntax to glue together several I/O actions into one.

{- >> helloname.hs <<

    main = do
        putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn ("Hey " ++ name ++ ", you rock!")

-}

-- Using the do syntax, we lay out a series of steps like we would in an imperative
-- program. Each step is an I/O action, which are all glued together into one
-- I/O action.  The action we got has a type of IO (), because that is the type
-- of the last I/O action inside.

-- main always has a type signature of main :: IO something, where something
-- is some concrete type. We do not usually specify a type declaration for main.

-- Examining the type of getLine, we see it has a type of getLine :: IO String

-- The <- construct binds the contents of the value returned by an I/O action to
-- the variable name. getLine has a type of IO String so name will have a type
-- of String.

-- If we are taking data out of an I/O action, we can only take it out when we
-- are inside of another I/O action. This is how Haskell separates pure and
-- impure parts of our code. getLine is tained with the IO type constructor
-- and we can only get that data out in I/O code. Because I/O code is tainted
-- too, any computation that depends on tainted I/O data will have a tained result.

-- That being said, when we are binding the value returned by getLine using the <-
-- construct to the name variable, we are temporarily un-taining the data. This allows
-- us to apply pure functions to this data.  In the example below, we could apply
-- a function like tellForture to the name variable, returned by getLine

{-

    main = do
        putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name

-}

-- tellFortune (and any of the functions it passes name to) doesn't have to know
-- anything about I/O, it is just a normal String -> String function

-- So for example, the following would not be valid

{-
    nameTag = "Hello, my name is " ++ getLine
-}

-- The left side of ++ has a type of String and the right sideh as a type of
-- IO String. This does not work since ++ requires the same type for both sides.
-- We would have to bind the value of getLine to a variable using <-

-- Every I/O action has a result encapsulated within it.  It would be possible to
-- write something like

{-

    main = do
        foo <- putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn ("Hey " ++ name ++ ", you rock!")

-}

-- However, foo has a type of (). We also do not bind the last putStrLn to anything
-- because the last action of a do block cannot be bound to a name.

-- Technically speaking, every line in a do block that doesn't bind can also be
-- written with a bind (putStrLn "Blah" can be _ <- putStrLn "Blah")

-- You may think that doing something like
--
--      name = getLine
--
-- will read from input and bind the value of that to name. All this does is give
-- the getLine I/O action a different name (called name). To get the value of an
-- I/O action, you have to perform it inside of another I/O action by binding
-- it to a name with <-

-- let bindings can be used in do blocks similarly to how they are used in list
-- comprehensions.

{- >> firstlast.hs <<

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-}

-- Now we will make a program that continuously reads a line and prints the same
-- line with the words reverse. The execution stops with a blank line input.

{- >> reversewords.hs <<

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-}

-- In the above main function, there are a few things worth noting.

-- In an I/O do block, if-statements have a form of
--  if condition then I/O action else I/O action

-- The line with return on it does not act like return in most imparative languages.
-- return in Haskell makes an I/O action out of a pure value. In the box analogy,
-- it takes a value and wraps in in a (I/O) box. The resulting I/O action does
-- not do anything, just has a value encapsulated as its result.

-- So, in an I/O context, return "haha" has a type of IO String

-- In our program we needed some I/O action to carry out in the case of an empty
-- line. Using return does not cause the I/O do block to end execution. For example,
-- the following program will execute until the last line:

{-

main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

-}

-- Since return is not bound to a name with <-, the result in each case above is
-- thrown away without being used. Contrast with the following:

{-

main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

-}

-- return is sort of the opposite to <-. While return takes a value and wraps
-- it up in a box, <- takes a box (and performs it) and takes the value out of
-- it, binding it to a name. But this is redundant; in the above example it
-- would be better to use let bindings in do blocks to bind to names:

{-

main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b

-}

-- In I/O blocks, we use return either because we need to create an I/O action
-- that doesn't do anything or because we don't want to modify the value of an
-- I/O action made in a do block from the value of its last action to a different
-- one.


-- Now, let's look at functions useful for dealing with I/O

{-

    putStr      - like putStrLn (takes string and returns I/O action that prints
                  string to terminal), except it doesn't jump to a new line

    putChar     - takes a character and returns an I/O action that will print
                  it out to the terminal

    print       - takes a value of any type that's an instance of Show (capable
                  of being represented as a string), calls show with that value
                  to stringify it, and then outputs that string to the terminal
                  (same as doing putStrLn . show). When we want to print strings,
                  we usually use putStrLn because we don't want the quotes around
                  them, but for printing out values of other types to the
                  terminal, print is used the most

    getChar     - I/O action that reads a character from input

    when        - takes a boolean and an I/O action. If that boolean is True, it
                  returns the same I/O action supplied to it. If false, it returns
                  the return () action. See when_test.hs

    sequence    - takes a list of I/O actions and returns an I/O action that will
                  perform those actions sequentially. The result in the new I/O
                  action will be a list of the results of all the I/O actions
                  performed. typically used when mapping functions like print or
                  putStrLn over lists

    mapM/mapM_  - utility functions related to sequencing. mapM takes a function
                  and a list, maps the function over the list, and then sequences
                  it. mapM_ does the same, only throws away the result later
                  (this removes the list of I/O actions that print, getLine, etc
                  return)

    forever     - takes an I/O action and returns an I/O action that just repeats
                  the I/O action it got forever. see forever_test.hs

    forM        - is like mapM, only that it has its paramters switched around.
                  the first parameter is the list and the second one is the
                  function to map over the list. useful in instances of use of
                  lambdas and do notation. see forM_test.hs

-}

{---------------------}
{- FILES AND STREAMS -}
{---------------------}

-- We've seen getChar and getLine (I/O actions that read a character/line from
-- input, respectively).  Now we will meet getContents, which is an I/O action
-- that reads everything from standard input until it encounters an EOF character.

-- getContents has type of getContents :: IO String.  getContents does lazy I/O,
-- meaning that it will read from input only as needed.

-- To demonstrate how getContents works, we'll repurpose our earlier program that
-- used the forever function.

{- >> capslocker.hs

    import Control.Monad
    import Data.Char

    main = forever $ do
        l <- getLine
        putStrLn $ map toUpper l

-}

-- Now compiling (ghc --make capslocker) and running this by typing
--
-- $ cat haiku.txt | ./capslocker
--
-- will result in the file using haiku.txt as input.  But we can make this better
-- by using getContents.

{- >> capslocker2.hs

    import Data.Char

    main = do
        contents <- getContents
        putStr (map toUpper contents)

-}

-- Now let's make a program that takes inputs and only prints out lines that are
-- shorter than 10 characters:

{- >> shortlines.hs

    main = do
        contents <- getContents
        putStr (shortLinesOnly contents)

    shortLinesOnly :: String -> String
    shortLinesOnly input =
        let allLines = lines input
            shortLines = filter (\line -> length line < 10) allLines
            result = unlines shortLines
        in  result

-}

-- The I/O part of the program is as short as possible.  We implemented it by
-- reading the input contents, running a function on them, and then printing
-- out what the function gave back.

-- The shortLinesOnly function works as follows:
--      1. It takes a string "short\nlooooooooooong\nshort again"
--      2. It uses the `lines` function to convert the string to a list of strings
--      3. It passes this list through an anonymous filter function
--      4. It converts the resulting list back into an output string
--      5. It returns the result

-- The pattern of getting a string from input, transforming it with a function,
-- and then outputting that is so common that there exists a function to make
-- doing this easier, called interact

-- interact takes a function of type String -> String as a parameter and returns
-- an I/O action that will take some input, run that function on it, and then print out the function's result. Let's modify our program to use that

{- >> shortlines2.hs

    main = interact shortLinesOnly

    shortLinesOnly :: String -> String
    shortLinesOnly input =
        let allLines = lines input
            shortLines = filter (\line -> length line < 10) allLines
            result = unlines shortLines
        in  result

-}

-- In fact, this can be achieved with much less code (at the cost of readability)
-- and to demonstrate our function composition:

{- >> shortlines3.hs

    main = interact $ unlines . filter ((<10) . length) . lines

-}

-- Now let's make a program that continuously reads a line and tells us if the
-- line is a palindrome or not. One option would be to use getLine, determine
-- whether the input is a palindrome, then run main again. But this is simplified
-- by using interact.

{- >> isPalindrome.hs

    respondPalindromes contents = unlines (map (\xs ->
     if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
        where isPalindrome xs = xs == reverse xs

-}

-- It is worth nothing that even thoughw e made a program that transforms one
-- big string of input into another, it acts like a program that does it line
-- by line.  Haskell is lazy and wants to print the first line of the result
-- string, and does so the moment it has the first line of input.

-- READING AND WRITING FILES

-- One way to think about reading from the terminal is to imagine that it's like
-- reading from a special file (stdin). Same goes for writing to it (stdout)

-- Let's start out with a program that just reads from a file and prints it out
-- to the terminal.

{- >> girlfriend.hs

    import System.IO

    main = do
        handle <- openFile "girlfriend.txt" ReadMode
        contents <- hGetContents handle
        putStr contents
        hClose handle

-}

-- Let's go over this line by line. Our program is several actions glued together
-- with a do block.
--
--      1. In the first line, we use a new function called openFile with type:
--
--          openFile :: FilePath -> IOMode -> IO Handle
--
--         FilePath is just a type synonym for String.
--         IOMode is a type that represents what we want to do with our file.
