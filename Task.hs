module Task  where

import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Perm

data Task = Task {
    tText :: String
  , tDeadline :: String
  , tSchedule :: String
                 }

fields :: [(String, Task->String)]
fields = [ ("TASK", tText)
         , ("DEADLINE", tDeadline)
         , ("SCHEDULE", tSchedule)
         ]

instance Show Task where
  show t = (concat . intersperse " | " . map ($ t)) [tText, tDeadline, tSchedule]

instance Eq Task where
  a == b = tText a == tText b


serialize :: Task -> String
serialize t = (unlines . filter (/="") . map
               (\(k, f) -> let res = f t in
                             if res /= "" then
                               k ++ " " ++ res
                             else "")) fields


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
     result <- many1 (noneOf "\n")
     many eol
     return result


eol :: Parser Char
eol = char '\n'
