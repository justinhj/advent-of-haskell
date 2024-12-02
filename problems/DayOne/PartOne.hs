{-|
module: DayOne.PartOne
description: Advent of Code, Day One, Part One
-}
module DayOne.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Data.List (sort)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("input", 3714264),
             ("example", 11) ]

-- TODO use the authors parsing here or is it overkill?
-- TODO optionally use Result here
parseLine :: ([Int], [Int]) -> String -> ([Int], [Int])
parseLine (l1, l2) line = case map read (words line) :: [Int] of
  [n1, n2] -> (n1 : l1, n2 : l2)
  _ -> error "Invalid input format. Please provide exactly two numbers."

parseAll :: String -> ([Int], [Int])
parseAll input = foldl parseLine ([],[]) (lines input)

solve :: ([Int], [Int]) -> Result Int
solve (l1,l2) = let s1 = sort l1
                    s2 = sort l2
                    in
  Right (foldl (\acc (x, y) -> acc + abs (x - y)) 0 (zip s1 s2))

-- | Solution for Day One, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parseAll) solve
