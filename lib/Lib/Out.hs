{-|
module: Lib.Out
description: Printing the results of AoC attempts.
-}
module Lib.Out(display, test) where

import Lib.Types
import System.FilePath(takeFileName)

{-|
This takes an array of TestResults, and a final Result and prints them.
Notably, it does not print any attempt if any of the tests fail, allowing
us to skip long running problems if there's clearly an error.
-}
display :: Answer a => [TestResult] -> Result a -> IO ()
display tests result = test tests >> putStrLn testGuard
    where testGuard = if passing tests then resultMsg else "Please fix issues with tests before attempting on main input"
          resultMsg = case result of
            Left msg -> "There was an error on the main input: " ++ msg
            Right out -> toString out

{-|
This just prints out the tests.
-}
test :: [TestResult] -> IO()
test = mapM_ (putStrLn . showResult) 

-- | Pretty print a TestResult
showResult :: TestResult -> String
showResult (name, Nothing) = "Test " ++ takeFileName name ++ " passed."
showResult (name, Just msg) = "Test " ++ takeFileName name ++ " failed: " ++ msg

-- | Check if the tests are all passing (i.e. - returned Nothing)
passing :: [TestResult] -> Bool
passing = foldr ((&&) . ( pass . snd)) True
    where pass (Just _) = False
          pass Nothing = True
