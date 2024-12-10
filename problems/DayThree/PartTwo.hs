{-|
module: DayThree.PartTwo
description: Advent of Code, Day Three, Part Two
-}
module DayThree.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution (always)
import Data.Char (isDigit)

-- | The type of the answer to this problem
type Out = Int

data Mul = Mul Int Int

examples :: [(String, Out)]
examples = [("input",  97529391),
            ("example1", 161),
            ("example2", 48),
            ("test1", 10001),
            ("test2", 10001),
            ("test3", 60)]

data ParseState = Nostate | LNum | RNum | M | U | L | Comma | LB | DO_D | DO_O | DO_LB | DONT_N | DONT_' | DONT_T | DONT_LB
  deriving Eq

getMuls :: ([Mul],ParseState,String,String,Bool) -> [Mul]
getMuls (muls,_,_,_,_) = muls

type Acc = ([Mul], ParseState, String, String, Bool)

parser :: Acc -> Char -> Acc
parser (muls, state, left, right, ok) c = case state of
  Nostate -> case c of
    'm' ->
      if ok then
        (muls, M, "", "", ok)
      else
        resetState
    'd' ->
      (muls, DO_D, "", "", ok)
    _ -> resetState
  M -> case c of
    'u' -> (muls, U, "", "", ok)
    _ -> resetState
  U -> case c of
    'l' -> (muls, L, "", "", ok)
    _ -> resetState
  L -> case c of
    '(' -> (muls, LB, "", "", ok)
    _   -> resetState
  LB -> 
    if isDigit c then
      (muls, LNum, c : left, right, ok)
    else resetState
  LNum -> 
    if isDigit c && length left < 3 then
      (muls, LNum, c : left, right, ok)
    else if c == ',' then
      (muls, Comma, left, right, ok)
    else resetState
  Comma ->
    if isDigit c then
      (muls, RNum, left, c : right, ok)
    else resetState
  RNum -> 
    if isDigit c && length right < 3 then
      (muls, RNum, left, c : right, ok)
    else if c == ')' then
      (muls ++ [Mul (read (reverse left)) (read (reverse right))], Nostate, "", "", ok)
    else 
      resetState
  DO_D -> case c of
    'o' -> (muls, DO_O, "", "", ok)
    _   -> resetState
  DO_O ->
    if c == '(' then (muls, DO_LB, "", "", ok)
    else if c == 'n' then (muls, DONT_N, "", "", ok)
    else resetState
  DO_LB -> case c of
    ')' -> (muls, Nostate, "", "", True)
    _   -> resetState
  DONT_N -> case c of
    '\'' -> (muls, DONT_', "", "", ok)
    _   -> resetState
  DONT_' -> case c of
    't' -> (muls, DONT_T, "", "", ok)
    _   -> resetState
  DONT_T -> case c of
    '(' -> (muls, DONT_LB, "", "", ok)
    _   -> resetState
  DONT_LB -> case c of
    ')' -> (muls, Nostate, "", "", False)
    _   -> resetState
  where
      resetState = (muls, Nostate, "", "", ok)

parse :: String -> [Mul]
parse s = getMuls parsed
  where
    parsed = foldl parser ([],Nostate,"","",True) s

mult :: Mul -> Int
mult (Mul l r) = l * r  

solve :: String -> Int
solve input = sum $ map mult (parse input)

-- Note it may be nicer to use the author's parser than a hand coded state machine lol
-- also consider making the parsing step part of the parsing step instead of the solution? 

-- | Solution for Day Three, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always id) (always solve)
