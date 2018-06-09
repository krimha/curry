import System.Environment
import System.Directory
import System.IO
import Data.List
import Text.ParserCombinators.Parsec
import Task
import Data.Either
import Control.Monad.Trans

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           , ("update", update)
           ]


curryEnv :: IO String
curryEnv = getEnv "CURRY"


main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


list :: [String] -> IO ()
list [] = do
  fileName <- curryEnv
  result <- parseFromFile tasks fileName
  case result of
    Left err -> putStrLn $ show err
    Right ts -> sequence_ $ map putStr taskStr
      where
        taskStr = zipWith (\n t -> (show n) ++ " | " ++ (show t) ++ "\n") [1..] ts


add :: [String] -> IO ()
add [taskText] = do
  fileName <- curryEnv
  appendFile fileName $ serialize (Task taskText "" "" )

remove :: [String] -> IO ()
remove [numberString] = do
  fileName <- curryEnv
  result <- parseFromFile tasks fileName
  let taskNumber = read numberString :: Int
      upDated = case result of
                  Left e -> [] -- TODO: Alert error
                  Right ts -> if and [taskNumber > 0, taskNumber <= length ts]
                    then delete (ts !! (taskNumber-1)) ts else ts
  (tempName, tempHandler) <- openTempFile "." "temp"
  hPutStr tempHandler $ concat (map serialize upDated)
  hClose tempHandler
  removeFile fileName
  renameFile tempName fileName

getTasks = do
  fileName <- curryEnv
  result <- parseFromFile tasks fileName 
  let tasks = case result of
                Left e -> [] -- TODO: Alert error
                Right ts -> ts
  return tasks

writeTasks ts = do
  fileName <- curryEnv
  (tempName, tempHandler) <- openTempFile "." "temp"
  hPutStr tempHandler $ concat (map serialize ts)
  hClose tempHandler
  removeFile fileName
  renameFile tempName fileName

update :: [String] -> IO ()
update [numberString,field,newValue] = do
  tasks <- getTasks
  let taskNumber = read numberString :: Int
      (firstPart,t:secondPart) = splitAt (taskNumber-1) tasks
      updatedTask = case field of
                      "text"     -> t { tText     = newValue }
                      "deadline" -> t { tDeadline = newValue }
                      "schedule" -> t { tSchedule = newValue }
      updatedTasks = firstPart ++ updatedTask:secondPart
  writeTasks updatedTasks
