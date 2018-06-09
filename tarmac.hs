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
           ]


-- TODO: Find a more elegant solution
fileNameIO :: IO String
fileNameIO = getEnv "CURRY"


main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


list :: [String] -> IO ()
list [] = do
  fileName <- fileNameIO
  result <- parseFromFile tasks fileName
  case result of
    Left err -> putStrLn $ show err
    Right ts -> sequence_ $ map putStr taskStr
      where
        taskStr = zipWith (\n t -> (show n) ++ " | " ++ (show t) ++ "\n") [1..] ts


add :: [String] -> IO ()
add [taskText] = do
  fileName <- fileNameIO
  appendFile fileName $ serialize (Task taskText "" "" )

remove :: [String] -> IO ()
remove [numberString] = do
  fileName <- fileNameIO
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
