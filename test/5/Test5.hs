module Test5 (test5) where

import qualified DayFive.PartOne as P1
import qualified DayFive.PartTwo as P2
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


test2 :: String
test2 = "75,47,61,53,29\n\
        \97,61,53,29,13\n\
        \75,29,13\n\
        \75,97,47,61,53\n\
        \61,13,29\n\
        \97,13,75,29,47\n"


parsed = P1.parseLines test1
expectedSet_61 = Just $ Set.fromList [13,29,53]

parsed2 = P1.parseLines2 test2

test5 :: Test
test5 = TestList [
    TestCase (assertBool "Parsed" (isRight parsed)),
    TestCase $
      case parsed of
        Right m -> assertEqual "Correct set elements" expectedSet_61 (Map.lookup 61 m)
        Left err -> assertFailure ("Parsing failed: " ++ show err),
    TestCase $
      case parsed2 of
        Right l -> assertEqual "test2 has right number of sequences" 6 (length l)
        Left err -> assertFailure ("Parsing failed: " ++ show err),
    TestCase $
      case (parsed, parsed2) of
        (Right afters, Right seqs) -> assertEqual "s1 good" True (P1.verify afters [] (head seqs))
        _ -> assertFailure "Parsing failed",
    TestCase $
      case (parsed, parsed2) of
        (Right afters, Right seqs) -> assertEqual "s4 bad" False (P1.verify afters [] (seqs !! 4))
        _ -> assertFailure "Parsing failed"
        ]
