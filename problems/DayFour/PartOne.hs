{-|
module: DayFour.PartOne
description: Advent of Code, Day Four, Part One
-}
module DayFour.PartOne(Out, solution, parse, solve) where

import Lib.Solution
import Helpers.Solution
import Data.Array

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [
    ("example", 18),
    ("input", 2458)
  ]

listTo2DArray :: [[a]] -> Array (Int, Int) a
listTo2DArray xs = listArray ((0, 0), (rows - 1, cols - 1)) (concat xs)
  where
    rows = length xs
    cols = length (head xs)

parseLine :: String -> [Int]
parseLine = map charToInt
  where
    charToInt 'X' = 0
    charToInt 'M' = 1
    charToInt 'A' = 2
    charToInt 'S' = 3
    charToInt _   = -1

parse :: String -> Array (Int, Int) Int
parse content = listTo2DArray $ map parseLine (lines content)

testNext :: Array (Int, Int) Int -> Int -> Int -> Int -> Int -> Int -> Int
testNext arr row col dRow dCol target_val
  | not (inRange (bounds arr) (row, col)) = 0  -- Out of bounds
  | arr ! (row, col) /= target_val        = 0  -- Target value not found
  | target_val == 3                       = 1  -- Success when target_val is 3
  | otherwise                             = testNext arr (row + dRow) (col + dCol) dRow dCol (target_val + 1)

-- This is called for each element in the array
-- It counts the number of XMAS emanating from the element
testEl :: Array (Int, Int) Int -> Int -> Int -> Int
testEl arr row col = if val == 0 then
        testNext arr (row - 1) (col - 1) (-1) (-1) 1 +  -- Top-left
        testNext arr (row - 1) col       (-1)  0  1 +  -- Top
        testNext arr (row - 1) (col + 1) (-1)  1  1 +  -- Top-right
        testNext arr row       (col - 1)  0  (-1) 1 +  -- Left
        testNext arr row       (col + 1)  0   1  1 +  -- Right
        testNext arr (row + 1) (col - 1)  1  (-1) 1 +  -- Bottom-left
        testNext arr (row + 1) col        1   0  1 +  -- Bottom
        testNext arr (row + 1) (col + 1)  1   1  1    -- Bottom-right
                     else 0
  where
    val = arr ! (row, col)

sumElements :: Array (Int, Int) Int -> Int 
sumElements arr = sum [testEl arr row col | (row, col) <- indices arr]

solve :: Array (Int, Int) Int -> Int
solve = sumElements

-- | Solution for Day Four, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parse) (always solve)
