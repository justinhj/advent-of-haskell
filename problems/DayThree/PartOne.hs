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
examples = [("input", 10), ("example", 161)]

data ParseState = Nostate | LNum | RNum | M | U | L | Comma | LB
  deriving Eq

getMuls :: ([Mul],ParseState,String,String) -> [Mul]
getMuls (muls,_,_,_) = muls

type Acc = ([Mul], ParseState, String, String)

parser :: Acc -> Char -> Acc
parser (muls, state, left, right) c = case state of
  Nostate -> case c of
    'm' -> (muls, M, "", "")
    _ -> (muls, Nostate, "", "")
  M -> case c of
    'u' -> (muls, U, "", "")
    _ -> (muls, Nostate, "", "")
  U -> case c of
    'l' -> (muls, L, "", "")
    _ -> (muls, Nostate, "", "")
  L -> case c of
    '(' -> (muls, LB, "", "")
    _   -> (muls, Nostate, "", "")
  LB -> 
    if isDigit c
      then (muls, LNum, c : left, right)
      else (muls, Nostate, "", "")
  LNum -> 
    if isDigit c && length left < 3 then
      (muls, LNum, c : left, right)
    else if c == ',' then
      (muls, Comma, left, right)
    else (muls, Nostate, "", "")
    -- mul(12,345)
  Comma ->
    if isDigit c
      then (muls, RNum, left, c : right)
      else (muls, Nostate, "", "")
  RNum -> 
    if isDigit c && length right < 3 then
      (muls, RNum, left, c : right)
    else if c == ')' then
      (muls ++ [Mul (read left) (read right)], Nostate, "", "")
    else 
      (muls, Nostate, "", "")

parse :: String -> [Mul]
parse s = getMuls parsed
  where
    parsed = foldl parser ([],Nostate,"","") s

mult :: Mul -> Int
mult (Mul l r) = l * r  

solve :: String -> Int
solve input = sum $ map mult (parse input)

-- | Solution for Day Three, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always id) (always solve)
