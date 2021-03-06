import System.Environment
import System.Directory
import System.IO
import Data.List
import Text.ParserCombinators.Parsec
import Task
import Data.Either
import Control.Monad.Trans



main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           , ("update", update)
           , ("help", help)
           ]


list :: [String] -> IO ()
list [] = do
  fileName <- curryEnv
  result <- parseFromFile tasks fileName
  case result of
    Left err -> putStrLn $ show err
    Right ts -> putStr $ prettyShow ts


add :: [String] -> IO ()
add [taskText] = do
  fileName <- curryEnv
  appendFile fileName $ serialize (Task taskText "" "" )


remove :: [String] -> IO ()
remove [numberString] = do
  ts <- getTasks
  let taskNumber = read numberString :: Int
      updated = if and [taskNumber > 0, taskNumber <= length ts]
                    then delete (ts !! (taskNumber-1)) ts
                else ts
  writeTasks updated

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

help :: [String] -> IO ()
help _ = putStrLn $ unlines [ "Usage:"
                            , ""
                            , "\tcurry list"
                            , "\tcurry add TEXT"
                            , "\tcurry remove NUMBER"
                            , "\tcurry update NUMBER FIELD NEWVAULE" ]


curryEnv :: IO String
curryEnv = getEnv "CURRY"


pad :: [String] -> [String]
pad xs = map (\x -> x++padding x) xs
  where maxlen = (maximum . map length) xs
        padding x = (replicate (maxlen-(length x)) ' ')


prettyShow :: [Task] -> String
prettyShow ts = unlines $ (head finishedRows:separator:tail finishedRows)
  where
    headers = ["ID","TASK","DEADLINE","SCHEDULED"]
    decoratedRows = headers : zipWith (\t n -> (show n) : map ($ t) fields) ts [1..]
    padded  = (transpose . map pad . transpose) decoratedRows
    separator = (concat . intersperse "-|-" . map (flip replicate '-' . length)) $ head padded
    finishedRows = map (concat . intersperse " | ") padded

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
