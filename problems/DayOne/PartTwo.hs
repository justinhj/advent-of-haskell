{-|
module: DayOne.PartTwo
description: Advent of Code, Day One, Part Two
-}
module DayOne.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Control.Monad (foldM)
-- import Data.List (sort)
import qualified Data.Map as Map

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("input", 18805872),
             ("example", 31) ]

parseLine :: ([Int], [Int]) -> String -> Result ([Int], [Int])
parseLine (l1, l2) line = case map read (words line) :: [Int] of
  [n1, n2] -> Right (n1 : l1, n2 : l2)
  _ -> Left "Invalid input format. Please provide exactly two numbers."

parseAll :: String -> Result ([Int], [Int])
parseAll input = foldM parseLine ([],[]) (lines input)

countOccurences :: Int -> [Int] -> Int
countOccurences a = foldl (\acc b -> acc + (if a == b then 1 else 0)) 0 

occurences :: [Int] -> [Int] -> Map.Map Int Int
occurences l1 l2 = foldl (\acc a -> Map.insertWith const a (countOccurences a l2) acc) Map.empty l1

solve :: ([Int], [Int]) -> Result Int
solve (l1,l2) = 
  let
    occs = occurences l1 l2
    vals = foldl (\acc a -> acc + (a * Map.findWithDefault 0 a occs)) 0 l1
  in
    Right vals

-- | Solution for Day One, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseAll solve
