{-

    Learn You a Haskell - Chapter 9

    Input and Output

    notes

-}

import System.IO
import System.Random
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

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
--
--         IOMode is a type that represents what we want to do with our file.
--
--          data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
--
--         It will return an I/O action that opens the specified file in
--         the specified mode.  If we bind that action to something we get
--         a Handle (which represents where our file is).
--
--      2. In the next line, hGetContents takes a Handle and returns an IO String
--
--         This is the same as getContents except it takes a file handle rather
--         than standard (terminal) input.  The function will read the file
--         as needed.
--
--      3. putStr contents will print the contents to std out
--
--      4. hClose takes a handle and returns an I/O action that closes the file
--

-- An alternative to doing all of this is to use the withFile function
--
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--
-- It takes a path to a file, an IOMode, and then it takes a function that
-- takes a handle and returns some I/O action.  It returns an I/O action
-- that opens that file, does something to it, and closes it.

{- girlfriend-withFile.hs

main = do
    withFile "girlFriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

-}

-- Let's implement our own withFile function:

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-- Like hGetContents in relation to getContents for a specific file,
-- we also have hGetLine, hPutStr, hPutStrLn, hGetChar, etc.

-- However, if our goal is to load files and treat their contents as strings,
-- this is a common operation and we have functions to make our work easier:
--
--  readFile :: FilePath -> IO String
--
-- readFile takes a path to a file and returns an I/O action that will read
-- the file and bind its contents to something as a string. It replaces
-- using openFile and then doing hGetContents

{- girlfriend-readFile.hs

    main = do
        contents <- readFile "girlfriend.txt"
        putStr contents

-}

--  writeFile :: FilePath -> String -> IO ()
--
--  Takes a path to a file and a string to write to that file and returns an
--  I/O action that will do the writing.  It will overwrite existing files

{- girlFriend-writeFile.hs

    main = do
        contents <- readFile "girlfriend.txt"
        writeFile "girlfriendcaps.txt" (map toUpper contents)

-}

-- appendFile :: FilePath -> String -> IO ()
--
-- Works just like writeFile, except appends to a file rather than overwriting it

{- todo.hs

    main = do
        todoItem <- getLine
        appendFile "todo.txt" (todoItem ++ "\n")

-}


-- Since we mentioned that using hGetContents is lazy, it is worth noting that
-- using this function is like connecting a pipe from the file to the output.
--
-- We can think of files as streams (like we think of lists).  This will read
-- one line at a time and print it to the terminal as it goes along.
--
-- Text files are usually line-buffered.  Binary files are block-buffered.
--
-- We can control how exactly buffering is done using the hSetBuffering function
--
-- It takes a BufferMode and returns an I/O action that sets the buffering.
--
--  data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
--
-- Here is our earlier withFile example with block buffering

{-

    main = do
        withFile "something.txt" ReadMode (/handle -> do
            hSetBuffering handle $ BlockBuffering (Just 2048)
            contents <- hGetContents handle
            putStr contents)

-}

-- Reading files in bigger chunks minimizes disk access or when our file
-- is a slow network resource.
--
-- We can also use the hFlush function to dump the contents of the handle
-- and free the data to make it available to other programs running simultaneously

-- Now let's make a program to remove from our todo list

{- todoRemove.hs

    main = do
        handle <- openFile "todo.txt" ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"
        contents <- hGetContents handle
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
        putStrLn "These are your todo items:"
        putStr $ unlines numberedTasks
        putStrLn "Which one do you want to delete?"
        numberString <- getLine
        let number = read numberString
            newTodoItems = delete (todoTasks !! number) todoTasks
        hPutStr tempHandle $ unlines newTodoItems
        hClose handle
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt"

-}

{--------------------------}
{- COMMAND LINE ARGUMENTS -}
{--------------------------}

-- The System.Environment module has two basic I/O actions:
--
--   getArgs :: IO [String] - will get the arguments that the program was run
--   with and contain a result with a list of the args
--
--   getProgName :: IO String - contains the program name
--

{- command-line.hs

    import System.Environment
    import Data.List

    main = do
       args <- getArgs
       progName <- getProgName
       putStrLn "The arguments are:"
       mapM putStrLn args
       putStrLn "The program name is:"
       putStrLn progName

-}

{--------------}
{- RANDOMNESS -}
{--------------}

-- import System.Random

-- In most languages, you can call a function that will give you back a pseudo-random
-- number.  Since Haskell has referential transparency, any function that takes a certain
-- set of parameters must always produce the same result. This changes how randomness
-- must be handled in Haskell.

-- The System.Random module gives us facilities for randomness.  It includes the
-- following functions:
--
--      random :: (RandomGen g, Random a) => g -> (a, g)
--
--          - The RandomGen typeclass is for types that can act as a source of
--            randomness
--          - The Random typeclass is for things that can take random values
--            (e.g. a boolean, a number, but not a function)
--
--      mkStdGen :: Int -> StrGen
--
--          - Takes a number and gives us a random generator.
--
--      randoms :: (RandomGen g, Random a) => g -> [a]
--
--          - Takes a generator and returns an infinite sequence of values
--            based on that generator.
--
--      randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
--
--          - Takes as its first parameter a pair of values that set the lower
--            and upper bounds of the final value produced
--
--      randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]
--
--          - Produces a stream of random values within our defined range
--

-- Let's use random and mkStdGen together to give us a (hardly) random number
-- Since random is a polymorphic function, we need to specify the output type
randomNumber = random (mkStdGen 100) :: (Int, StdGen)
    -- gives (-3633736515773289454,693699796 2103410263)

-- Now a random boolean
randomBoolean = random (mkStdGen 100) :: (Bool, StdGen)
    -- gives (True,4041414 40692)

-- Now a random float
randomFloat = random (mkStdGen 100) :: (Float, StdGen)
    -- gives (0.6512469,651872571 1655838864)

-- Let's make a function that simulates tossing a coin three times
-- We will use the generator that the random function outputs
-- in our successive calls to it
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen1)    = random gen
        (secondCoin, newGen2)   = random newGen1
        (thirdCoin, newGen3)    = random newGen2
    in (firstCoin, secondCoin, thirdCoin)

-- can be invoked using `threeCoins (mkStdGen 100)`, etc.

-- We did not specify an output type to the random gen calls (e.g. random gen :: (Bool, StdGen))
-- because we already specified that we wanted booleans in the function type declaration.

-- There is a function called randoms that takes a generator and returns
-- an infinite sequence of values based on that generator


fiveRandomNumbers = take 5 $ randoms (mkStdGen 11) :: [Int]

fiveRandomBools = take 5 $ randoms (mkStdGen 11) :: [Bool]

fiveRandomFloats = take 5 $ randoms (mkStdGen 11) :: [Float]

-- The randoms function cannot give us back a generator because it has to potentially
-- generate an infinite list of numbers, so we can't give the new random generator back

-- We can make a function that generates a finite stream of numbers and a new generator
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)

-- Suppose we want a random value in some range.  We can use randomR for that.
dieRoll = randomR (1,6) (mkStdGen 65535) :: (Int, StdGen)

-- Or we can use randomRs to produce a random stream within our defined ranges
randomString = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

-- Everything we have done so far involves seeding our random number generator
-- with a hard coded value in our code. Problem is that in real code, this
-- would always produce the same results. This is where I/O comes into play.

-- We can use the (getStdGen :: IO StdGen) function to retrieve a random number
-- generator from the system when we bind it to something.

{- random-number.hs

    import System.Random

    main = do
        gen <- getStdGen
        putStr $ take 20 (randomRs ('a','z') gen)

-}

-- Note that performing getStdGen twice will ask for the same global generator twice
-- We should use (newStdGen :: IO StdGen), which updates the global generator from
-- the initial getStdGen call.

-- Let's write a program that will make the user guess which number it's thinking of.

{- random-guess.hs

    import System.Random
    import Control.Monad(when)

    main = do
        gen <- getStdGen
        askForNumber gen

    askForNumber :: StdGen -> IO ()
    askForNumber gen = do
        let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
        putStr "Which number in the range from 1 to 10 am I thinking of? "
        numberString <- getLine
        when (not $ null numberString) $ do
            let number = read numberString
            if randNumber == number
                then putStrLn "You are correct!"
                else putStrLn $ "Sorry, the number was " ++ show randNumber
            askForNumber newGen

-}

{---------------}
{- BYTESTRINGS -}
{---------------}

-- import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString as S

-- Lists are cool, and they benefit from laziness in many situations. However,
-- because the String type is implemented as list of Chars, the default String
-- type in Haskell is slow.

-- The overhead is particularly a problem when reading and manipulating big files.
-- Haskell's bytestring type comes to the rescue. Bytestrings are similar to lists,
-- ecept that each element is one byte (8 bits). They also handle laziness
-- differently.

-- Strict bytestrings (Data.ByteString) represent a series of bytes in an array.
-- You cannot have things like infinite strict bytestrings. If you evaluate the
-- first byte, you have to evaluate it whole. The upside with this implementation
-- is that there is no thunks, but the tradeoff is that they are likely to fill
-- up memory faster.

-- Lazy bytestrings (Data.ByteString.Lazy) are not quite az lazy as lists. In a
-- list, there as as many thunks as there are elements, which sometimes makes
-- them slow. Lazy bytestrings are stored in chunks (not thunks) of size 64K.
-- So when a byte in a lazy bytestring is evaluated, the first 64K will be
-- evaluated.

-- We have several functions that are similar to Data.List functions in the
-- ByteString modules.
--
--      pack :: [Word8] -> ByteString
--
--          Takes a list of bytes and turns it into a bytestring
--
--      unpack :: ByteString -> [Word8]
--
--          Takes a bytestring and turns it into a list of bytes
--
--      fromChunks :: [Data.ByteString] -> Data.ByteString.Lazy
--
--          Takes a list of strict bytestrings and converts to a lazy one
--
--      toChunks :: Data.ByteString.Lazy -> [Data.ByteString]
--
--          Takes a lazy bytestring and converts to a list of strict ones
--
--      cons :: GHC.Word.Word8 -> Data.ByteString.Lazy -> Data.ByteString.Lazy
--
--          Takes a byte and a bytestring and puts the byte at the bginning.
--          Since it is lazy, it will make a new chunk even if the first chunk
--          is not full.
--
--      cons' :: GHC.Word.Word8 -> Data.ByteString.Lazy -> Data.ByteString.Lazy
--
--          The strict version of cons. Better to use for lots of insertions.
--
--      Other functions similar to ones in Data.List include head, tail, init,
--      null, length, map, reverse, foldl, foldr, concat, takeWhile, filter, etc.
--
--      readFile :: FilePath -> IO ByteString
--
--          Similar to readFile in the System.IO module, reads the file into
--          chunks (if lazy) or entirely into memory (if strict).

packedList = B.pack [99,97,110]

packedListRange = B.pack [98..120]

-- We are going to write a program that takes two filenames as CLI arguments
-- and copies the first file into the second file.

{- bytestring-copy.hs

    import System.Environment
    import qualified Data.ByteString.Lazy as B

    main = do
        (fileName1:fileName2:_) <- getArgs
        copyFile fileName1 fileName2

    copyFile :: FilePath -> FilePath -> IO ()
    copyFile source dest = do
        contents <- B.readFile source
        B.writeFile dest contents

-}

-- Note that the program that does not use bytestrings looks very similar.
-- Sometimes conversion of a program from normal to bytestrings just requires
-- doing the necessary imports and putting qualified module names in front
-- of some functions.

{--------------}
{- EXCEPTIONS -}
{--------------}

-- Haskell's type system allows us to handle unexpected values (e.g. Maybe),
-- but we may encounter issues with pure code that does not use Maybe.
-- This is where exceptions may be thrown (e.g. 4 `div` 0)

-- However, exceptions even in pure code must be handled in IO code (e.g.
-- in the main function's do block).  This creates a tension between keeping
-- main slim and keeping code pure.

-- We will look at errors in IO actions.  Consider the following:

{-

    import System.Environment  
    import System.IO  
      
    main = do (fileName:_) <- getArgs  
              contents <- readFile fileName  
              putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

-}

-- If we run this function and fileName does not exist, we will get an error and
-- program crash.  One alternative is to use the doesFileExist function.

{-

    import System.Environment  
    import System.IO  
    import System.Directory  

    main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
              else do putStrLn "The file doesn't exist!"  

-}

-- doesFileExist has a type of FilePath -> IO Bool, so we need to use the
-- "draw from" `<-` construct

-- To deal with exceptions, we need to use the `catch` function. It takes two
-- parameters, an I/O action and an exception handler.

-- Here's the same functionailty as above, except we put the main function
-- through a catch handler

{-

    import System.Environment  
    import System.IO  
    import System.IO.Error  
      
    main = toTry `catch` handler  
                  
    toTry :: IO ()  
    toTry = do (fileName:_) <- getArgs  
               contents <- readFile fileName  
               putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
      
    handler :: IOError -> IO ()  
    handler e = putStrLn "Whoops, had some trouble!"  

-}

-- The handler function can include checks for specific errors with the
-- corresponding functions

{-

    ... same as above

    handler :: IOError -> IO ()  
    handler e  
        | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
        | otherwise = ioError e

-}

-- There are several error predicates that we can use with IOError:
--   - isAlreadyExistsError
--   - isDoesNotExistError
--   - isAlreadyInUseError
--   - isFullError
--   - isEOFError
--   - isIllegalOperation
--   - isPermissionError
--   - isUserError

