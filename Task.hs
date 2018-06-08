
module Task  where

import Text.ParserCombinators.Parsec

data Task = Task {
  tText :: String
                 }

instance Show Task where
  show task = unlines
    [ tText task ]

instance Eq Task where
  a == b = tText a == tText b


serialize :: Task -> String
serialize task = "TASK " ++ (tText task) ++ "\n"



tasks :: Parser [Task]
tasks =
  do result <- many task
     eof
     return result

task :: Parser Task
task =
  do spaces
     string "TASK"
     spaces
     result <- line
     eol
     return Task { tText=result }

line :: Parser String
line = many (noneOf "\n")

eol :: Parser Char
eol = char '\n'



