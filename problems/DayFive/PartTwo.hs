{-|
module: DayFive.PartTwo
description: Advent of Code, Day Five, Part Two
-}
module DayFive.PartTwo(Out, solution, shouldSwap, fix, swapElements) where

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

examples :: [(String, Out)]
examples = [("test1", 123), ("input",0)]

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

findIntersection :: (Ord a) => Set.Set a -> [a] -> Set.Set a
findIntersection set lst = Set.intersection set (Set.fromList lst)

shouldSwap :: Map.Map Int (Set.Set Int) -> [Int] -> [Int] -> Maybe (Int,Int)
shouldSwap after visits input = go visits input
  where
    go _ [] = Nothing
    go visited (x:xs) =
        case Map.lookup x after of
            Nothing -> go (x:visited) xs
            Just mustComeAfter ->
              let intersection = findIntersection mustComeAfter visited in
              if not (Set.null intersection) then
                Just (x, Set.findMin intersection)
              else
                go (x:visited) xs

sumMiddles :: [[Int]] -> Int
sumMiddles = sum . map (fromMaybe 0 . middle)
  where
    middle :: [Int] -> Maybe Int
    middle xs
      | null xs   = Nothing
      | otherwise = Just $ xs !! (length xs `div` 2)

fix :: Map.Map Int (Set.Set Int) -> [Int] -> [Int]
fix seconds s = case shouldSwap seconds [] s of
  Nothing -> s
  Just (i1, i2) -> 
    let swapped = swapElements s i1 i2
    in fix seconds swapped

swapElements :: [Int] -> Int -> Int -> [Int]
swapElements xs a b = map swap xs
  where
    swap x
      | x == a = b
      | x == b = a
      | otherwise = x

fixAll :: Map.Map Int (Set.Set Int) -> [[Int]] -> [[Int]]
fixAll seconds = map (fix seconds)

solve :: (Map.Map Int (Set.Set Int), [[Int]]) -> Out
solve (seconds, seqs) = sumMiddles $ fixAll seconds filtered
  where
    filtered = filter (not . verify seconds []) seqs

-- | Solution for Day Five, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseInput (always solve)
