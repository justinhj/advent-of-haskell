{-|
module: DayFive.PartOne
description: Advent of Code, Day Five, Part One
-}
module DayFive.PartOne(Out, solution, parseInput, parseLines, parseLines2) where

import Lib.Solution
import Lib.Types hiding (Parser)
import Lib.Types hiding (Parser)
import Data.List.Split (splitOn)
import Helpers.Solution
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String

-- | The type of the answer to this problem
type Out = Int

lineParser :: Parser (Int, Int)
lineParser = do
    left <- many1 digit
    _ <- char '|'
    right <- many1 digit
    return (read left, read right)

linesParser :: Parser [(Int, Int)]
linesParser = many (lineParser <* newline)

toMap :: [(Int, Int)] -> Map.Map Int (Set.Set Int)
toMap = Map.fromListWith Set.union . map (\(a, b) -> (b, Set.singleton a))

parseLines :: String -> Either ParseError (Map.Map Int (Set.Set Int))
parseLines input = case parse linesParser "" input of
    Left err -> Left err
    Right pairs -> Right (toMap pairs)

intParser :: Parser Int
intParser = read <$> many1 digit

lineParser2 :: Parser [Int]
lineParser2 = do
    numbers <- intParser `sepBy` char ','
    _ <- newline
    return numbers

-- Parser for multiple lines, resulting in [[Int]]
linesParser2 :: Parser [[Int]]
linesParser2 = many lineParser2

-- Main parsing function
parseLines2 :: String -> Either ParseError [[Int]]
parseLines2 = parse linesParser2 ""

examples :: [(String, Out)]
examples = [("test1", 143)]

parseInput :: String -> Result Int
parseInput input = Right 10

-- | Solution for Day Five, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseInput (nyi "Solution")
