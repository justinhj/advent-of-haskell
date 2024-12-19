{-|
module: DayFour.PartOne
description: Advent of Code, Day Four, Part One
-}
module DayFour.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Data.Array

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [
    ("test1", 4)
  ]


parse :: String -> [[Int]]
parse content = map (map read . words) (lines content)

-- | Solution for Day Four, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parse) (nyi "Solution")
