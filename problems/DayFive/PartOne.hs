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

parseInput :: String -> Result (Map.Map Int (Set.Set Int), [[Int]])
parseInput input = do
    let (first, second) = case splitOn "\n\n" input of
                            [] -> ("", "")
                            [x] -> (x, "")
                            (x:y:_) -> (x, y)
    mapped <- either (Left . show) Right (parseLines (first ++ "\n"))
    seqs <- either (Left . show) Right (parseLines2 second)
    return (mapped, seqs)

verify :: Map.Map Int (Set.Set Int) -> [Int] -> [Int] -> Bool
verify after visits input = go visits input
  where
    go _ [] = True  -- Base case: if the input list is empty, return True
    go visited (x:xs) = 
        case Map.lookup x after of
            Nothing -> go (x:visited) xs  -- If x isn't in after, just proceed
            Just mustComeAfter -> 
                if any (`elem` visited) mustComeAfter 
                then False  -- If any number that must come after x has already been seen, return False
                else go (x:visited) xs  -- Otherwise, proceed by adding x to visited

solve :: (Map.Map Int (Set.Set Int), [[Int]]) -> Out
solve (seconds, seqs) = length seqs -- filtered
  where
    filtered = filter (\a -> verify seconds [] a) seqs

-- | Solution for Day Five, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseInput (always solve)
