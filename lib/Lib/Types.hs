{-|
module: Lib.Types
description: Some types that are used consistently in the Framework.
-}
module Lib.Types where

{-|
Each step in our chain can either error (resulting in a string message that
can be printed), or be successful (resulting in a value).
-}
type Result a = Either String a

{-|
Parsers are the first step in your AoC solution.  They should take a string
(The contents of the input file), and can return the parse as a result.
representation of the file, or a string error.
-}
type Parser a = String -> Result a

{-|
Solvers are the second and final step in your AoC solution.  They should take
whatever type your parser spits out, and return a result of the desired
type.  It's important that this result type has an implementation of Show.
-}
type Solver a b = a -> Result b

{-|
Sometimes Advent of Code likes to have an additional argument in its examples
which is different for the real thing.  Think like "number of iterations" for
some game or another.  In this case, we provide a function which provides a
solver based on that input.
-}
type SolverConfigurable a b c = a -> Solver b c

{-|
Test cases are based on a file that _looks_ like input from the puzzle.
Provide as a string the name of the example as it is labeled in the example
folder.  This will be combined with information on the day in question to
read the correct file and pass the input to your parser.  The second element
is the expected solution given that input.  This must match the type
output by your solver on.
-}
type TestCase a = (String, a)

{-|
In this triple, the first element is still the name of the example file.  The
expected result is moved to the last item, and in between is the configurable
input.
-}
type TestCaseConfigurable a b = (String, a, b)

{-|
The result of a test - The name of the test file, and a possible Error message.
Nothing indicates that the test was successful.
-}
type TestResult = (String, Maybe String)

{-|
An Answer to an AoC Problem.  This utility class allows any thing that
implements "show" to be used.  Strings are handled specially, and
this allows us to recover that information.
-}
class Answer a where
    toString :: a -> String

-- | Strings should be directly printed
instance {-# OVERLAPPING #-} Answer String where
    toString = id

-- | Anything (except Sring) that implements Show can be printed that way.
instance Show a => Answer a where
    toString = show
