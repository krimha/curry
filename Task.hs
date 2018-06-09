
module Task  where

import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Perm

data Task = Task {
    tText :: String
  , tDeadline :: String
  , tSchedule :: String
                 }

instance Show Task where
  show t = (concat . intersperse " | " . map ($ t)) [tText, tDeadline, tSchedule]

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
task = do t <- keywordLine "TASK"
          let tuple d s = (d,s)
          (d,s) <- permute
                   (tuple <$?> ("", keywordLine "DEADLINE")
                          <|?> ("", keywordLine "SCHEDULE"))
          return Task { tText=t, tDeadline=d, tSchedule=s}



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
