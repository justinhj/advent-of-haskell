{-|
module: Lib.Solution
description: The Main Functions for bundling up your solutions.
-}
module Lib.Solution (
    AdventProblem,
    adventOfCode,
    adventOfCodeConfigurable
)where

import Lib.Types
import Lib.Processing
import System.FilePath((</>))

{-|
A function that takes a base input directory, and runs tests, and
runs the solution against the actual input, then returns all
of those.
-}
type AdventProblem a = String -> IO ([TestResult], Result a)

{-|
Generate a standard AdventProblem.
-}
adventOfCode :: Answer a => Eq a
             => [TestCase a] -- ^ Test cases and expected output
             -> Parser b -- ^ The Parser for this problem
             -> Solver b a  -- ^ The solver for this Problem
             -> AdventProblem a
adventOfCode testCases parse solve baseDir = (,) <$> testResults <*> result
    where testResults = test parse solve resolvedTestCases
          result = readAndApply (dirname </> "input") $ process parse solve
          resolvedTestCases = map (\(name, res) -> (dirname </> name, res)) testCases
          dirname = baseDir

{-|
Generate a configurable AdventProblem.  For instance, often the example runs an
operation 1, or 5, or 50 times, then expects the actual problem to run the
iteraiton 10k times.  Using this, we can provide that single configuration
variable.
-}
adventOfCodeConfigurable :: Answer b => Eq b
                         => [TestCaseConfigurable a b] -- ^ Test Cases and expected output
                         -> a -- ^ What the configuration input should be for the main input
                         -> Parser c -- ^ The Parser for this problem
                         -> SolverConfigurable a c b -- ^ The solver for this problem, with config arg
                         -> AdventProblem b
adventOfCodeConfigurable testCases arg parse mkSolver baseDir = (,) <$> testResults <*> result
    where testResults = testConfigurable parse mkSolver resolvedTestCases
          result = readAndApply (dirname </> "input") $ process parse (mkSolver arg)
          resolvedTestCases = map (\(name, a, res) -> (dirname </> name, a, res)) testCases
          dirname = baseDir
