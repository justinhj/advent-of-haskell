{-|
module: DayFour.PartTwo
description: Advent of Code, Day Four, Part Two
-}
module DayFour.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution
import Data.Array

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [
    ("example", 9),
    ("input", 1945)
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

probe :: Array (Int, Int) Int -> Int -> Int -> Int -> Bool
probe arr row col target
  | not (inRange (bounds arr) (row, col)) = False
  | arr ! (row, col) == target = True
  | otherwise = False

-- There are only four possible MAS's 
-- 1   3     1   3
--  2   2   2   2
--   3   1 3   1

-- M is 1 and S is 3
validXMas :: Array (Int, Int) Int -> Int -> Int -> Int
validXMas arr row col = if (a || b) && (c || d) then 1 else 0
  where
    a = probe arr (row - 1) (col - 1) 1 && probe arr (row + 1) (col + 1) 3
    b = probe arr (row - 1) (col - 1) 3 && probe arr (row + 1) (col + 1) 1
    c = probe arr (row - 1) (col + 1) 1 && probe arr (row + 1) (col - 1) 3
    d = probe arr (row - 1) (col + 1) 3 && probe arr (row + 1) (col - 1) 1

-- for each element if it is an A (2) then check for surrounding MAS's
testEl :: Array (Int, Int) Int -> Int -> Int -> Int
testEl arr row col = if val == 2 then
                      validXMas arr row col
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
