{-|
module: DayTwo.PartTwo
description: Advent of Code, Day Two, Part Two
-}
module DayTwo.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("example", 4),
              ("input", 400) ]

parseLine :: [[Int]] -> String -> Result [[Int]]
parseLine nums line =
  let
    these = mapMaybe readMaybe (words line)
  in 
  if null these 
    then Left "Parse error"
    else Right (these : nums)

parseAll :: String -> Result [[Int]]
parseAll input = foldM parseLine [] (lines input)

removeNth :: Int -> [a] -> [a]
removeNth n xs = take n xs ++ drop (n + 1) xs

getRemovals :: [Int] -> [[Int]]
getRemovals l = map (`removeNth` l) [0..(length l-1)] 

getDiffs :: [Int] -> [Int]
getDiffs z = map (uncurry (-)) zipped
  where 
    zipped = zip z (tail z) 

combineInputsWithRemovals :: [[Int]] -> [[[Int]]]
combineInputsWithRemovals = concatMap (\x -> [x : getRemovals x])

allSameSign :: [Int] -> Bool
allSameSign [] = False
allSameSign (x:xs) = all (\a -> signum a == signum x) xs

allGradualChange :: [Int] -> Bool
allGradualChange l = (minimum abs_vals >= 1) && (maximum abs_vals <=3)
  where abs_vals = map abs l

validateInput :: [Int] -> Bool
validateInput xs = allSameSign diffs && allGradualChange diffs
  where diffs = getDiffs xs

validateGroup :: [[Int]] -> Bool
validateGroup = any validateInput

solve :: [[Int]] -> Result Int
solve xss = Right $ length goodLists
  where 
    goodLists = filter validateGroup (combineInputsWithRemovals xss)

-- | Solution for Day Two, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseAll solve
