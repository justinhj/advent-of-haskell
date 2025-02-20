{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-|
module: DaySix.PartOne
description: Advent of Code, Day Six, Part One
-}
module DaySix.PartOne(Out, solution) where

import Lib.Solution
import Helpers.Solution
import Data.Array

-- | The type of the answer to this problem
type Out = Int

data Loc = EMPTY | BLOCKED | GUARD | VISITED
    deriving (Show, Eq)

data Dir = N | E | S | W
    deriving (Show, Eq)

parseMap :: String -> [[Loc]]
parseMap str = map (map charToLoc) (lines str)
  where
    charToLoc '.' = EMPTY
    charToLoc '#' = BLOCKED
    charToLoc '^' = GUARD
    charToLoc _   = EMPTY -- Keep the pattern matching exhaustive

listTo2DArray :: [[a]] -> Array (Int, Int) a
listTo2DArray xs = listArray ((0, 0), (rows - 1, cols - 1)) (concat xs)
  where
    rows = length xs
    cols = length (head xs)

examples :: [(String, Out)]
examples = [("test", 41), ("input", 5239)]

parse :: String -> Array (Int, Int) Loc
parse content = listTo2DArray $ parseMap content

guardStart :: Array (Int, Int) Loc -> (Int, Int)
guardStart arr = case [(i, j) | ((i, j), loc) <- assocs arr, loc == GUARD] of
        [] -> error "No guard found in array"
        (pos:_) -> pos

score :: Array (Int, Int) Loc -> Int
score arr = length [(i, j) | ((i, j), loc) <- assocs arr, loc == VISITED]

search :: (Int, Int) -> Dir -> Array (Int, Int) Loc -> (Int, Array (Int, Int) Loc)
search spot d m
  | not (inRange (bounds m) spot) = (score m, m)
  | ifForwardBlocked spot = search spot (turnRight d) m
  | otherwise = let newSpot = moveForward spot d in
                search newSpot d (m // [(spot, VISITED)])
  where
      moveForward (i, j) N = (i - 1, j)
      moveForward (i, j) E = (i, j + 1)
      moveForward (i, j) S = (i + 1, j)
      moveForward (i, j) W = (i, j - 1)

      turnRight N = E
      turnRight E = S
      turnRight S = W
      turnRight W = N

      ifForwardBlocked (i, j)
        = let newSpot = moveForward (i, j) d
          in
            if not (inRange (bounds m) newSpot) then
                False
            else
                let content = m ! newSpot in
                content == BLOCKED

solve :: Array (Int, Int) Loc -> Int
solve m = fst (search (guardStart m) N m)

solution:: AdventProblem Out
solution = adventOfCode examples (always parse) (always solve)
