module Test4 (test4) where

import DayFour.PartOne (parse)
import Test.HUnit
import Data.Array

testInput1 :: String
testInput1 = "MMMSXXMASM\n\
             \MSAMXMSMSA\n\
             \AMXSXMAAMM\n\
             \MSAMASMSMX\n\
             \XMASAMXAMM\n\
             \XXAMMXXAMA\n\
             \SMSMSASXSS\n\
             \SAXAMASAAA\n\
             \MAMMMXMMMM\n\
             \MXMXAXMASX\n"

parsed = parse testInput1

test4 :: Test
test4 = TestList [
  TestCase (assertEqual "M is 1" 1 (parsed ! (0,0))),
  TestCase (assertEqual "X is 0" 0 (parsed ! (0,4))),
  TestCase (assertEqual "A is 2" 2 (parsed ! (1,9))),
  TestCase (assertEqual "S is 3" 3 (parsed ! (1,1)))
  ]


