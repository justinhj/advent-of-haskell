{-|
module: DayFour.PartOne
description: Advent of Code, Day Four, Part One
-}
module DayFour.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Data.Array
import Debug.Trace

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [
    ("test1", 4)
  ]

listTo2DArray :: [[a]] -> Array (Int, Int) a
listTo2DArray xs = listArray ((0, 0), (rows - 1, cols - 1)) (concat xs)
  where
    rows = length xs
    cols = length (head xs)


parse :: String -> Array (Int, Int) Int
parse content = listTo2DArray $ map (map read . words) (lines content)

solve :: Array (Int, Int) Int -> Int
solve input = trace (show input) 4

-- | Solution for Day Four, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parse) (always solve)
