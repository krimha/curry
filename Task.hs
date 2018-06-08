
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
  do text <- keywordLine "TASK"
     return Task { tText=text }

-- Parses a line on the form `prefix content`, with arbitrary
-- whitespace in front of and after both the line, and the keyword
keywordLine :: String -> Parser String
keywordLine s =
  do many eol
     spaces
     string s
     spaces
     result <- many (noneOf "\n")
     many eol
     return result

eol :: Parser Char
eol = char '\n'
