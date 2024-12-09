{-|
module: DayTwo.PartOne
description: Advent of Code, Day Two, Part One
-}
module DayTwo.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Helpers.Solution (always)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("example", 2),
              ("input", 334) ]

parseLine :: [[Int]] -> String -> Result [[Int]]
parseLine nums line = let
  these = mapMaybe readMaybe (words line)
    in 
  if null these 
    then Left "Parse error"
    else Right (these : nums)

parseAll :: String -> Result [[Int]]
parseAll input = foldM parseLine [] (lines input)

getDiffs :: [Int] -> [Int]
getDiffs z = map (uncurry (-)) zipped
  where 
    zipped = zip z (tail z) 

allSameSign :: [Int] -> Bool
allSameSign [] = False
allSameSign (x:xs) = all (\a -> signum a == signum x) xs

allGradualChange :: [Int] -> Bool
allGradualChange l = (minimum abs_vals >= 1) && (maximum abs_vals <=3)
  where abs_vals = map abs l

solve :: [[Int]] -> Result Int
solve = always $ length . filter (\xs -> allSameSign xs && allGradualChange xs) . map getDiffs

-- | Solution for Day Two, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseAll solve
