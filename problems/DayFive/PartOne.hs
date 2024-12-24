{-|
module: DayFive.PartOne
description: Advent of Code, Day Five, Part One
-}
module DayFive.PartOne(Out, solution, parseInput, parseLines, parseLines2) where

import Lib.Solution
import Lib.Types hiding (Parser)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Bifunctor as Bifunctor
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
-- Originally toMap = Map.fromListWith Set.union . map (\(a, b) -> (a, Set.singleton b))
toMap = Map.fromListWith Set.union . map (Bifunctor.second Set.singleton)

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
examples = [("test1", 143), ("input",5452)]

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
    go _ [] = True
    go visited (x:xs) =
        case Map.lookup x after of
            Nothing -> go (x:visited) xs
            Just mustComeAfter ->
                not (any (`elem` visited) mustComeAfter) && go (x:visited) xs

sumMiddles :: [[Int]] -> Int
sumMiddles = sum . map (fromMaybe 0 . middle)
  where
    middle :: [Int] -> Maybe Int
    middle xs
      | null xs   = Nothing
      | otherwise = Just $ xs !! (length xs `div` 2)

solve :: (Map.Map Int (Set.Set Int), [[Int]]) -> Out
solve (seconds, seqs) = sumMiddles filtered
  where
    filtered = filter (verify seconds []) seqs

-- | Solution for Day Five, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseInput (always solve)
