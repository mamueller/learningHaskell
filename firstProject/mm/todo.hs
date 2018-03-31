import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
            , ("view",view)
            , ("remove",remove)
            ]

main = do 
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName,todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
     handle <- openFile fileName ReadMode
     (tempName,tempHandle) <-openTempFile "." "temp"
     contents <-hGetContents handle
     let todoTasks = lines contents
         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)[0..] todoTasks
     putStrLn "These are your TO-DO items:"
     putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName,numberString] = do
    handle <- openFile fileName ReadMode
    (tempName,tempHandle) <-openTempFile "." "temp"
    contents <-hGetContents handle
    let todoTasks = lines contents
        number = read numberString
        newTodoItems = delete (todoTasks !! number ) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName 
    renameFile tempName fileName 
-- main = do
--     todoItem <- getLine
--     appendFile "todo.txt" (todoItem ++ "\n")

-- main = do 
--     handle <- openFile "todo.txt" ReadMode
--     (tempName,tempHandle) <-openTempFile "." "temp"
--     contents <-hGetContents handle
--     let todoTasks = lines contents
--         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)[0..] todoTasks
--     putStrLn "These are your TO-DO items:"
--     putStr $ unlines numberedTasks
--     putStrLn "Which one do you want to delete?"
--     numberString <-getLine
--     let number = read numberString
--         newTodoItems = delete (todoTasks !! number ) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile "todo.txt"
--     renameFile tempName "todo.txt"
