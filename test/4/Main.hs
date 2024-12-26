module Test5 (test5) where

import DayFour.PartOne (parse)

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

-- test parsing the source into an easy format for the exercise
-- test1 :: Test
-- test1 = TestList [
--   TestCase assertEqual "M is 1"   


