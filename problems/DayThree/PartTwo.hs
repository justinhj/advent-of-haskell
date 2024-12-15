{-|
module: DayThree.PartTwo
description: Advent of Code, Day Three, Part Two
-}
module DayThree.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution (always)
import Text.Parsec
import Text.Parsec.String (Parser)
import Lib.Types (Result)
-- import Debug.Trace (traceShow)

type Out = Int

examples :: [(String, Out)]
examples = [
            -- ("input",  97529391),
            -- ("example", 161),
            -- ("example2", 48),
            ("test1", 10001)
            -- ("test2", 10001),
            -- ("test3", 60)
            ]

data ParsedItem = Mul Int Int | On | Off
  deriving (Show, Eq)

number :: Parser Int
number = do
  digits <- many1 digit
  let ns = take 3 digits
  case length ns of
    0 -> fail "no digits"
    _ -> 
      if length ns <= 3 then return (read ns)
      else fail "too many digits"

-- Parser for Mul
mulParser :: Parser ParsedItem
mulParser = do
  _ <- string "mul("
  l <- number
  _ <- char ','
  r <- number
  _ <- char ')'
  return $ Mul l r

-- Parser for On
onParser :: Parser ParsedItem
onParser = do
  _ <- string "don't()"
  return On

-- Parser for Off
offParser :: Parser ParsedItem
offParser = do
  _ <- string "do()"
  return Off

-- Combine parsers to parse any of the patterns
parsedItemParser :: Parser ParsedItem
parsedItemParser = mulParser <|> onParser <|> offParser

-- Parser to find all occurrences of the patterns
findAllParsedItems :: Parser [ParsedItem]
findAllParsedItems = many (parsedItemParser <* many anyChar)

-- Function to run the parser
parseItems :: String -> Either ParseError [ParsedItem]
parseItems = parse findAllParsedItems ""

process :: (Int, Bool) -> ParsedItem -> (Int, Bool)
process (total, mode) item = 
  case item of
    On -> (total, True)
    Off -> (total, False)
    Mul l r -> 
      if mode then 
        (total + l*r, mode)
      else 
        (total, mode)

calculate :: [ParsedItem] -> Int
calculate items = fst calc
  where calc = foldl process (0, True) items

solve :: String -> Result Int
solve input = 
  case ps of
    Right items ->
      Right $ calculate items
    Left parseError ->
      error (show parseError)
  where
    ps = parseItems input

-- | Solution for Day Three, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always id) solve
