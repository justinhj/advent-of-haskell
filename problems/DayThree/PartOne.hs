{-|
module: DayThree.PartOne
description: Advent of Code, Day Three, Part One
-}
module DayThree.PartOne(Out, solution) where

import Lib.Solution
import Helpers.Solution (always)
import Data.Char (isDigit)

-- | The type of the answer to this problem
type Out = Int

data Mul = Mul Int Int

examples :: [(String, Out)]
examples = [("input", 168539636),
            ("example", 161),
            ("test1", 10001),
            ("test2", 10001),
            ("test3", 60)]

data ParseState = Nostate | LNum | RNum | M | U | L | Comma | LB
  deriving Eq

getMuls :: ([Mul],ParseState,String,String) -> [Mul]
getMuls (muls,_,_,_) = muls

type Acc = ([Mul], ParseState, String, String)

parser :: Acc -> Char -> Acc
parser (muls, state, left, right) c = case state of
  Nostate -> case c of
    'm' -> (muls, M, "", "")
    _ -> resetState
  M -> case c of
    'u' -> (muls, U, "", "")
    _ -> resetState
  U -> case c of
    'l' -> (muls, L, "", "")
    _ -> resetState
  L -> case c of
    '(' -> (muls, LB, "", "")
    _   -> resetState
  LB -> 
    if isDigit c then
      (muls, LNum, c : left, right)
    else resetState
  LNum -> 
    if isDigit c && length left < 3 then
      (muls, LNum, c : left, right)
    else if c == ',' then
      (muls, Comma, left, right)
    else resetState
  Comma ->
    if isDigit c then
      (muls, RNum, left, c : right)
    else resetState
  RNum -> 
    if isDigit c && length right < 3 then
      (muls, RNum, left, c : right)
    else if c == ')' then
      (muls ++ [Mul (read (reverse left)) (read (reverse right))], Nostate, "", "")
    else 
      resetState
  where
      resetState = (muls, Nostate, "", "")


parse :: String -> [Mul]
parse s = getMuls parsed
  where
    parsed = foldl parser ([],Nostate,"","") s

mult :: Mul -> Int
mult (Mul l r) = l * r  

solve :: String -> Int
solve input = sum $ map mult (parse input)

-- Note it may be nicer to use the author's parser than a hand coded state machine lol
-- also consider making the parsing step part of the parsing step instead of the solution? 

-- | Solution for Day Three, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always id) (always solve)
