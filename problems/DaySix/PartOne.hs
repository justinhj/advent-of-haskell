{-|
module: DaySix.PartOne
description: Advent of Code, Day Six, Part One
-}
module DaySix.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Data.Array

-- | The type of the answer to this problem
type Out = Int

data Loc = EMPTY | BLOCKED | GUARD_N | GUARD_S | GUARD_W | GUARD_E | VISITED
    deriving (Show, Eq)

parseMap :: String -> [[Loc]]
parseMap str = map (map charToLoc) (lines str)
  where
    charToLoc '.' = EMPTY
    charToLoc '#' = BLOCKED
    charToLoc '^' = GUARD_N
    charToLoc _   = EMPTY  -- Default case for any other character

listTo2DArray :: [[a]] -> Array (Int, Int) a
listTo2DArray xs = listArray ((0, 0), (rows - 1, cols - 1)) (concat xs)
  where
    rows = length xs
    cols = length (head xs)

examples :: [(String, Out)]
examples = [("test", 41)]

parse :: String -> Array (Int, Int) Loc
parse content = listTo2DArray $ parseMap content

-- | Solution for Day Six, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (nyi "Parsing" :: String -> Result Out) (nyi "Solution")
