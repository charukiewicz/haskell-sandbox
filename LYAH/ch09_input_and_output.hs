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

-- No, having the whole program be just one I/O action is limiting. We use do
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
