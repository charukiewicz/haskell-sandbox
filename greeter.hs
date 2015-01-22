-- Taken from an example on reddit
-- http://goo.gl/BhZJLd
--
-- Run with ghci:
--	ghci
--	:l greeter
--	runGreeter baseGreeter
--	runGreeter derivedGreeter

type Name = String
type Age = Int

data Greeter = 
    Greeter { askForName   :: IO Name
            , sayHello     :: Name -> IO ()
            , askForAge    :: IO Age
            , commentOnAge :: Age -> IO ()
            , sayGoodbye   :: Name -> IO ()
            }

runGreeter :: Greeter -> IO ()
runGreeter g = do name <- askForName g
                  sayHello g name
                  age <- askForAge g
                  commentOnAge g age
                  sayGoodbye g name

prompt :: String -> IO String
prompt str = do putStrLn str
                getLine

baseGreeter :: Greeter
baseGreeter = Greeter askForName' sayHello' askForAge' commentOnAge' sayGoodbye'
    where askForName' = prompt "What's your name?"
          sayHello' name = putStrLn ("Hello, " ++ name ++ "!")
          askForAge' = fmap read (prompt "What's your age?")
          commentOnAge' age 
              | age < 18 = putStrLn "You're so young!"
              | otherwise = putStrLn "You're so old!!!"
          sayGoodbye' name = putStrLn ("I hope you rot in hell, " ++ name ++ "!")

derivedGreeter :: Greeter
derivedGreeter = baseGreeter { sayGoodbye = override }
    where override _ = putStrLn "Have a nice day!"
