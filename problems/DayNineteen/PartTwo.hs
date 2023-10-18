{-|
module: DayNineteen.PartTwo
description: Advent of Code, Day Nineteen, Part Two
-}
module DayNineteen.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = []

-- | Solution for Day Nineteen, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Result Out) (nyi "Solution")
