{-|
module: DayOne.PartOne
description: Advent of Code, Day One, Part One
-}
module DayOne.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Helpers.Input (lineByLine)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ -- ("input", 20),
             ("example", 11) ]

parseLine :: String -> ([Int], [Int]) -> ([Int], [Int])
parseLine line (l1, l2) = case map read (words line) :: [Int] of
  [n1, n2] -> (n1 : l1, n2 : l2)
  _ -> error "Invalid input format. Please provide exactly two numbers."

parseAll :: String -> ([Int], [Int])
parseAll input = foldl (\x a -> parseLine a x) ([],[]) (lines input)

-- | Solution for Day One, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parseAll) (nyi "Solution")
