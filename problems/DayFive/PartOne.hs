{-|
module: DayFive.PartOne
description: Advent of Code, Day Five, Part One
-}
module DayFive.PartOne(Out, solution, parse) where

import Lib.Solution
import Lib.Types
import Data.List.Split (splitOn)
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("test1", 143)]

parse :: String -> Result Int
parse input = Right 10

-- | Solution for Day Five, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parse (nyi "Solution")
