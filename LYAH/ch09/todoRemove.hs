import System.IO
import System.Directory
import Data.List

main = do
    -- Open todo.txt and bind its handle
    handle <- openFile "todo.txt" ReadMode
    -- Open and bind the name/handle of temp file in current directory
    (tempName, tempHandle) <- openTempFile "." "temp"
    -- Bind contents of todo.txt
    contents <- hGetContents handle
    -- Convert contents into a list of strings
    let todoTasks = lines contents
        -- Zip each string in the list with a number (0,1,..)
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your todo items:"
    -- Print the contents of our zipped list as individual lines
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    -- Read a number from standard input
    numberString <- getLine
    let number = read numberString
        -- Delete the corresponding element from our tasks list
        newTodoItems = delete (todoTasks !! number) todoTasks
    -- Write our new todo list to our temp file
    hPutStr tempHandle $ unlines newTodoItems
    -- Close both files
    hClose handle
    hClose tempHandle
    -- Delete the original file
    removeFile "todo.txt"
    -- Rename the temp file to the original file
    renameFile tempName "todo.txt"
