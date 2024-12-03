{-|
module: DayOne.PartTwo
description: Advent of Code, Day One, Part Two
-}
module DayOne.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Control.Monad (foldM)
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

countOccurrences :: Int -> [Int] -> Int
countOccurrences a = foldl (\acc b -> acc + (if a == b then 1 else 0)) 0 
-- note chatty suggested this, but it is less efficient if simpler
-- countOccurrences a = length . filter (== a)

occurrences :: [Int] -> [Int] -> Map.Map Int Int
occurrences l1 l2 = foldl (\acc a -> Map.insertWith const a (countOccurrences a l2) acc) Map.empty l1

solve :: ([Int], [Int]) -> Result Int
solve (l1,l2) = 
  let
    occs = occurrences l1 l2
    val = foldl (\acc a -> acc + (a * Map.findWithDefault 0 a occs)) 0 l1
  in
    Right val

-- | Solution for Day One, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parseAll solve
