{-|
module: DayThirteen.PartTwo
description: Advent of Code, Day Thirteen, Part Two
-}
module DayThirteen.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day Thirteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Result Out) (nyi "Solution")
