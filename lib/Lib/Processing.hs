{-|
module: Lib.Processing
Descritption: lib-internal functions for processing solutions against Input.
-}
module Lib.Processing where

import Lib.Types

{-|
Apply a parser and a solution to an already read input.
-}
process :: Parser a -> Solver a b -> String -> Result b
process parse solve input = parse input >>= solve

{-|
Apply a parser and a solution to an already read input, and check against an
expected value.
-}
check :: Answer b => Eq b
    => b -- ^ The expected result
    -> Parser a -- ^ A Parser
    -> Solver a b -- ^ A Solver
    -> String -- ^ The input
    -> Maybe String -- ^ Nothing if the results matches.  A message otherwise.
check expected parse solve input = toResult $ process parse solve input
    where toResult (Left msg) = Just msg
          toResult (Right actual)
            | actual == expected = Nothing
            | otherwise = Just msg
                where msg = "Expected " ++ toString expected ++ ", but got " ++ toString actual

{-|
Read a file and pass it into a function, recovering the output.
-}
readAndApply :: String -> (String -> a) -> IO a
readAndApply fname = (<$> readFile fname)

{-|
Run a regular test case and return the result.
-}
test :: Answer b => Eq b
     => Parser a
     -> Solver a b
     -> [TestCase b]
     -> IO [TestResult]
test parse solve = mapM exec
    where exec (name, expected) = (name,) <$> result
            where result = readAndApply name $ check expected parse solve

{-|
Run a configurable test case and return the result
-}
testConfigurable :: Answer c => Eq c
                 => Parser a
                 -> SolverConfigurable b a c
                 -> [TestCaseConfigurable b c]
                 -> IO [TestResult]
testConfigurable parse mkSolver = mapM exec
    where exec (name, arg, expected) = (name,) <$> result
            where result = readAndApply name $ check expected parse $ mkSolver arg
