import System.Environment
import System.Directory
import System.IO
import Data.List
import Text.ParserCombinators.Parsec
import Task (Task, tasks)
import Data.Either

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           ]


fileName :: String
fileName = "todo.txt"


main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


list :: [String] -> IO ()
list [] = do
  result <- parseFromFile tasks fileName
  case result of
    Right ts -> sequence_ $ map putStr taskStr
      where
        taskStr = zipWith (\n t -> (show n) ++ " - " ++ (show t)) [1..] ts


add :: [String] -> IO ()
add [task] = appendFile fileName (task ++ "\n")


remove :: [String] -> IO ()
remove [numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  fileContents <- hGetContents handle
  let taskNumber = read numberString
      tasks = lines fileContents
      updatedTasks = delete (tasks !! (taskNumber-1)) tasks
  hPutStr tempHandle $ unlines updatedTasks
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
