module Test5 (test5) where

import DayFive.PartOne (parseInput, parseLines)
import Test.HUnit
import Data.Either (isRight)
import Data.Map (lookup)
import qualified Data.Set as Set
import qualified Data.Map as Map

test1 :: String
test1 = "47|53\n\
        \97|13\n\
        \97|61\n\
        \97|47\n\
        \75|29\n\
        \61|13\n\
        \75|53\n\
        \29|13\n\
        \97|29\n\
        \53|29\n\
        \61|53\n\
        \97|53\n\
        \61|29\n\
        \47|13\n\
        \75|47\n\
        \97|75\n\
        \47|61\n\
        \75|61\n\
        \47|29\n\
        \75|13\n\
        \53|13\n"

parsed = parseLines test1
expectedSet_61 = Just $ Set.fromList [97,47,75]

test5 :: Test
test5 = TestList [
    TestCase (assertBool "Parsed" (isRight parsed)),
    TestCase $
      case parsed of
        Right m -> assertEqual "Correct set elements" expectedSet_61 (Map.lookup 61 m)
        Left err -> assertFailure ("Parsing failed: " ++ show err)
  ]
