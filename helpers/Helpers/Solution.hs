{- |
Module: Helpers.Solution
Description: Helper functions to make the framework do work.
-}

module Helpers.Solution where

import Lib.Types

{-|
Short for "not yet implemented".  This can be used in either the parser or the
solution function in the main adventOfCode Functions.
-}
nyi :: String -> a -> Result b
nyi name = const . Left $ name ++ " Not Yet Implemented"

{-|
Short for "not yet implemented".  This can be used in either the parser or the
solution function in the main adventOfCode Functions.
-}
nyiConf :: String -> a -> b -> Result c
nyiConf name = const . const . Left $ name ++ " Not Yet Implemented"

{-|
If your solution cannot return an error, and therefore doesn't use Result,
use this to lift it into result
-}
always :: (a -> b) -> a -> Result b
always = (pure .)

{-|
As Always, but allows for a second argument for the configurableAdventOfCode Family
-}
alwaysConf :: (a -> b -> c) -> a -> b -> Result c
alwaysConf = doubleCompose pure
    where doubleCompose = (.) . (.)

{-|
Lift a Maybe into a Result with a meaningful error.
-}
maybeToResult :: String -> Maybe a -> Result a
maybeToResult _ (Just x) = Right x
maybeToResult s Nothing = Left s

