{-|
module: DayThree.PartOne
description: Advent of Code, Day Three, Part One
-}
module DayThree.PartOne(Out, solution) where

import Lib.Solution
import Helpers.Solution (always)

-- | The type of the answer to this problem
type Out = Int

data Mul = Mul Int Int

-- number :: PS.Parser String
-- number = P.count 1 P.digit P.<|> P.count 2 P.digit P.<|> P.count 3 P.digit

examples :: [(String, Out)]
examples = [("input", 10), ("example", 161)]

data ParseState = Nostate | Left | Right | M | U | L | Comma | LB | RB
  deriving Eq

getMuls :: ([Mul],ParseState,String,String) -> [Mul]
getMuls (muls,_,_,_) = muls

type Acc = ([Mul], ParseState, String, String)

parser :: Acc -> Char -> Acc
parser = undefined

parse :: String -> [Mul]
parse s = getMuls parsed
  where
    parsed = foldl parser ([],Nostate,"","") s

mult :: Mul -> Int
mult (Mul l r) = l * r  

solve :: String -> Int
solve input = (sum $ map mult (parse input))

-- | Solution for Day Three, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always id) (always solve)
