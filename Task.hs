
module Task (tasks) where

import Text.ParserCombinators.Parsec

data Task = Task {
  text :: String
                 }

instance Show Task where
  show task = text task

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
     return Task { text=result }

line :: Parser String
line = many (noneOf "\n")

eol :: Parser Char
eol = char '\n'



