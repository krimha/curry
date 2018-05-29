import System.Environment

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           ]


fileName :: String
fileName = "todo.txt"


main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


list :: [String] -> IO ()
list [] = do
  fileContent <- readFile fileName
  let tasks = lines fileContent
      numberedTasks = zipWith (\n t -> (show n) ++ " - " ++ t) [1..] tasks
  putStr $ unlines numberedTasks


add :: [String] -> IO ()
add [task] = appendFile fileName (task ++ "\n")

