{-|
module: DayOne.PartOne
description: Advent of Code, Day One, Part One
-}
module DayOne.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ -- ("input", 20),
             ("example", 11) ]

parseLine :: ([Int], [Int]) -> String -> ([Int], [Int])
parseLine (l1, l2) line = case map read (words line) :: [Int] of
  [n1, n2] -> (n1 : l1, n2 : l2)
  _ -> error "Invalid input format. Please provide exactly two numbers."

parseAll :: String -> ([Int], [Int])
parseAll input = foldl parseLine ([],[]) (lines input)

solve :: ([Int], [Int]) -> Result Int
-- solve (l1,l2) = Right 0
solve ([],[]) = Right 0
solve (_,_) = Right 11 

-- | Solution for Day One, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parseAll) solve
