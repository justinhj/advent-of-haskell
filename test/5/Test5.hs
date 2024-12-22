module Test5 (test5) where

import Test.HUnit

test5 :: Test
test5 = TestCase (assertEqual "for (foo 3)," 8 (foo 3))

foo :: Int -> Int
foo x = x + 5

