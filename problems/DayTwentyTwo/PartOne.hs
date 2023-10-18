{-|
module: DayTwentyTwo.PartOne
description: Advent of Code, Day TwentyTwo, Part One
-}
module DayTwentyTwo.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day TwentyTwo, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Result Out) (nyi "Solution")
