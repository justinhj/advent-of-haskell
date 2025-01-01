{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-|
module: DaySix.PartTwo
description: Advent of Code, Day Six, Part Two
-}
module DaySix.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution
import Data.Array
import qualified Data.Set as Set

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
examples = [("test", 6), ("input", 0)]

parse :: String -> Array (Int, Int) Loc
parse content = listTo2DArray $ parseMap content

guardStart :: Array (Int, Int) Loc -> (Int, Int)
guardStart arr = case [(i, j) | ((i, j), loc) <- assocs arr, loc == GUARD] of
        [] -> error "No guard found in array"
        (pos:_) -> pos

visitedPositions :: Array (Int, Int) Loc -> Set.Set (Int, Int)
visitedPositions arr = Set.fromList [(i, j) | ((i, j), loc) <- assocs arr, loc == VISITED]

score :: Array (Int, Int) Loc -> Int
score arr = length $ visitedPositions arr

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

searchStep :: (Int, Int) -> Dir -> Array (Int, Int) Loc -> ((Int,Int), Dir)
searchStep spot d m
  | not (inRange (bounds m) spot) = (spot, d)
  | ifForwardBlocked spot = searchStep spot (turnRight d) m
  | otherwise = let newSpot = moveForward spot d in
                (newSpot, d)
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

testBlockPositionHelper :: Array (Int, Int) Loc -> (Int, Int) -> Dir -> (Int, Int) -> Dir -> Bool
testBlockPositionHelper m spot1 d1 spot2 d2 
  | not (inRange (bounds m) spot1) = False
  | not (inRange (bounds m) spot2) = False
  | (newSpot1 == newSpot2) && (d1 == d2) = True
  | otherwise = testBlockPositionHelper m newSpot1 newD1 newSpot2 newD2

  where
    (newSpot1', newD1') = searchStep spot1 d1 m
    (newSpot1, newD1) = searchStep newSpot1' newD1' m
    (newSpot2, newD2) = searchStep spot2 d2 m

testBlockPosition :: Array (Int, Int) Loc -> (Int, Int) -> (Int, Int) -> Bool
testBlockPosition m gs bp = testBlockPositionHelper blockedMap gs N gs N 
  where
    blockedMap = m // [(bp, BLOCKED)]
    

solve :: Array (Int, Int) Loc -> Int
solve m = length $ filter id blockedPositions
  where 
    gs = guardStart m
    (_, filledMap) = search gs N m
    vps = Set.toList $ Set.delete gs (visitedPositions filledMap)
    blockedPositions = map (testBlockPosition m gs) vps

solution:: AdventProblem Out
solution = adventOfCode examples (always parse) (always solve)
