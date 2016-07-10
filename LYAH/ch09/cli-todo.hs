import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())] 
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("bump", bump)
            ]

main = do
    -- First we get the arguments and use pattern matching to bind the first
    -- argument to command, and the rest get bound to args
    (command:args) <- getArgs
    -- Now we lookup the command in the dispatch list (which may or may not
    -- exist, so we use Just)
    case lookup command dispatch of 
        Just action -> action args
        Nothing     -> errorExit                              

errorExit :: IO ()
errorExit = do
    putStrLn "Invalid command"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        bumpedTask = todoTasks !! number
        otherTasks = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ bumpedTask ++ "\n"
    hPutStr tempHandle $ unlines otherTasks
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
