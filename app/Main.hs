module Main where

import System.Environment (getArgs, lookupEnv)
import System.Directory
import System.FilePath
import GHC.Data.Maybe (orElse)
import Lib.Out
import Lib.Types
import qualified DayOne.PartOne
import qualified DayOne.PartTwo
import qualified DayTwo.PartOne
import qualified DayTwo.PartTwo
import qualified DayThree.PartOne
import qualified DayThree.PartTwo
import qualified DayFour.PartOne
import qualified DayFour.PartTwo
import qualified DayFive.PartOne
import qualified DayFive.PartTwo
import qualified DaySix.PartOne
import qualified DaySix.PartTwo
import qualified DaySeven.PartOne
import qualified DaySeven.PartTwo
import qualified DayEight.PartOne
import qualified DayEight.PartTwo
import qualified DayNine.PartOne
import qualified DayNine.PartTwo
import qualified DayTen.PartOne
import qualified DayTen.PartTwo
import qualified DayEleven.PartOne
import qualified DayEleven.PartTwo
import qualified DayTwelve.PartOne
import qualified DayTwelve.PartTwo
import qualified DayThirteen.PartOne
import qualified DayThirteen.PartTwo
import qualified DayFourteen.PartOne
import qualified DayFourteen.PartTwo
import qualified DayFifteen.PartOne
import qualified DayFifteen.PartTwo
import qualified DaySixteen.PartOne
import qualified DaySixteen.PartTwo
import qualified DaySeventeen.PartOne
import qualified DaySeventeen.PartTwo
import qualified DayEighteen.PartOne
import qualified DayEighteen.PartTwo
import qualified DayNineteen.PartOne
import qualified DayNineteen.PartTwo
import qualified DayTwenty.PartOne
import qualified DayTwenty.PartTwo
import qualified DayTwentyOne.PartOne
import qualified DayTwentyOne.PartTwo
import qualified DayTwentyTwo.PartOne
import qualified DayTwentyTwo.PartTwo
import qualified DayTwentyThree.PartOne
import qualified DayTwentyThree.PartTwo
import qualified DayTwentyFour.PartOne
import qualified DayTwentyFour.PartTwo
import qualified DayTwentyFive.PartOne

main :: IO ()
main = do
    args <- getArgs
    switch args

switch :: [String] -> IO()
switch ("puzzle":rest) = runAoC solve rest
switch ("test":rest) = runAoC runTests rest
switch (x:_) = putStrLn ("Unknown mode: " ++ x)
switch [] = putStrLn "Invoke with (puzzle|test) (day) (part)"

runAoC :: (String -> String -> String -> IO()) -> [String] -> IO()
runAoC f [day, "both"] = putStrLn ("Running Both problems for Day " ++ day) >> run "1" >> run "2"
    where run part = addHeadersAndSpace day part $ runAoC f [day, part]
runAoC f ["all"] = foldl (>>) (putStrLn "Running all Problems") solutions
    where solutions = map run days
          run (day, part) = addHeadersAndSpace day part $ runAoC f [day, part]
          days = [(show d, show p) | d <- [1..24] :: [Int], p <- [1..2] :: [Int]] ++ [("25", "1")]
runAoC f [day, part] = do
    inputDir <- getInputDir day
    f day part inputDir
runAoC _ _ = putStrLn "Requires exactly two arguments: Day, then Part"

addHeadersAndSpace :: String -> String -> IO() -> IO()
addHeadersAndSpace day part result = putStrLn hdr >> result >> putStrLn ""
    where hdr = "Day " ++ day ++ ", Part " ++ part

doDisplay :: Answer a => IO ([TestResult], Result a) -> IO ()
doDisplay = (uncurry display =<<)

doTest :: Answer a => IO ([TestResult], Result a) -> IO ()
doTest = (test =<<) . (fmap fst)

getInputDir :: String -> IO String
getInputDir day = combine <$> basePath <*> pure day
    where basePath = orElse <$> env <*> standard
          env = lookupEnv "AOC_INPUT_DIRECTORY"
          standard = (</> "examples") <$> getCurrentDirectory

-- I'm sure ther's a less explicit way to do this, but... I honestly don't know how to do it.
runTests :: String -> String -> String -> IO()
runTests "1" "1" = doTest . DayOne.PartOne.solution
runTests "1" "2" = doTest . DayOne.PartTwo.solution
runTests "2" "1" = doTest . DayTwo.PartOne.solution
runTests "2" "2" = doTest . DayTwo.PartTwo.solution
runTests "3" "1" = doTest . DayThree.PartOne.solution
runTests "3" "2" = doTest . DayThree.PartTwo.solution
runTests "4" "1" = doTest . DayFour.PartOne.solution
runTests "4" "2" = doTest . DayFour.PartTwo.solution
runTests "5" "1" = doTest . DayFive.PartOne.solution
runTests "5" "2" = doTest . DayFive.PartTwo.solution
runTests "6" "1" = doTest . DaySix.PartOne.solution
runTests "6" "2" = doTest . DaySix.PartTwo.solution
runTests "7" "1" = doTest . DaySeven.PartOne.solution
runTests "7" "2" = doTest . DaySeven.PartTwo.solution
runTests "8" "1" = doTest . DayEight.PartOne.solution
runTests "8" "2" = doTest . DayEight.PartTwo.solution
runTests "9" "1" = doTest . DayNine.PartOne.solution
runTests "9" "2" = doTest . DayNine.PartTwo.solution
runTests "10" "1" = doTest . DayTen.PartOne.solution
runTests "10" "2" = doTest . DayTen.PartTwo.solution
runTests "11" "1" = doTest . DayEleven.PartOne.solution
runTests "11" "2" = doTest . DayEleven.PartTwo.solution
runTests "12" "1" = doTest . DayTwelve.PartOne.solution
runTests "12" "2" = doTest . DayTwelve.PartTwo.solution
runTests "13" "1" = doTest . DayThirteen.PartOne.solution
runTests "13" "2" = doTest . DayThirteen.PartTwo.solution
runTests "14" "1" = doTest . DayFourteen.PartOne.solution
runTests "14" "2" = doTest . DayFourteen.PartTwo.solution
runTests "15" "1" = doTest . DayFifteen.PartOne.solution
runTests "15" "2" = doTest . DayFifteen.PartTwo.solution
runTests "16" "1" = doTest . DaySixteen.PartOne.solution
runTests "16" "2" = doTest . DaySixteen.PartTwo.solution
runTests "17" "1" = doTest . DaySeventeen.PartOne.solution
runTests "17" "2" = doTest . DaySeventeen.PartTwo.solution
runTests "18" "1" = doTest . DayEighteen.PartOne.solution
runTests "18" "2" = doTest . DayEighteen.PartTwo.solution
runTests "19" "1" = doTest . DayNineteen.PartOne.solution
runTests "19" "2" = doTest . DayNineteen.PartTwo.solution
runTests "20" "1" = doTest . DayTwenty.PartOne.solution
runTests "20" "2" = doTest . DayTwenty.PartTwo.solution
runTests "21" "1" = doTest . DayTwentyOne.PartOne.solution
runTests "21" "2" = doTest . DayTwentyOne.PartTwo.solution
runTests "22" "1" = doTest . DayTwentyTwo.PartOne.solution
runTests "22" "2" = doTest . DayTwentyTwo.PartTwo.solution
runTests "23" "1" = doTest . DayTwentyThree.PartOne.solution
runTests "23" "2" = doTest . DayTwentyThree.PartTwo.solution
runTests "24" "1" = doTest . DayTwentyFour.PartOne.solution
runTests "24" "2" = doTest . DayTwentyFour.PartTwo.solution
runTests "25" "1" = doTest . DayTwentyFive.PartOne.solution
runTests day part = const . putStrLn $ "No problem for day " ++ day ++ ", part " ++ part

solve :: String -> String -> String -> IO()
solve "1" "1" = doDisplay . DayOne.PartOne.solution
solve "1" "2" = doDisplay . DayOne.PartTwo.solution
solve "2" "1" = doDisplay . DayTwo.PartOne.solution
solve "2" "2" = doDisplay . DayTwo.PartTwo.solution
solve "3" "1" = doDisplay . DayThree.PartOne.solution
solve "3" "2" = doDisplay . DayThree.PartTwo.solution
solve "4" "1" = doDisplay . DayFour.PartOne.solution
solve "4" "2" = doDisplay . DayFour.PartTwo.solution
solve "5" "1" = doDisplay . DayFive.PartOne.solution
solve "5" "2" = doDisplay . DayFive.PartTwo.solution
solve "6" "1" = doDisplay . DaySix.PartOne.solution
solve "6" "2" = doDisplay . DaySix.PartTwo.solution
solve "7" "1" = doDisplay . DaySeven.PartOne.solution
solve "7" "2" = doDisplay . DaySeven.PartTwo.solution
solve "8" "1" = doDisplay . DayEight.PartOne.solution
solve "8" "2" = doDisplay . DayEight.PartTwo.solution
solve "9" "1" = doDisplay . DayNine.PartOne.solution
solve "9" "2" = doDisplay . DayNine.PartTwo.solution
solve "10" "1" = doDisplay . DayTen.PartOne.solution
solve "10" "2" = doDisplay . DayTen.PartTwo.solution
solve "11" "1" = doDisplay . DayEleven.PartOne.solution
solve "11" "2" = doDisplay . DayEleven.PartTwo.solution
solve "12" "1" = doDisplay . DayTwelve.PartOne.solution
solve "12" "2" = doDisplay . DayTwelve.PartTwo.solution
solve "13" "1" = doDisplay . DayThirteen.PartOne.solution
solve "13" "2" = doDisplay . DayThirteen.PartTwo.solution
solve "14" "1" = doDisplay . DayFourteen.PartOne.solution
solve "14" "2" = doDisplay . DayFourteen.PartTwo.solution
solve "15" "1" = doDisplay . DayFifteen.PartOne.solution
solve "15" "2" = doDisplay . DayFifteen.PartTwo.solution
solve "16" "1" = doDisplay . DaySixteen.PartOne.solution
solve "16" "2" = doDisplay . DaySixteen.PartTwo.solution
solve "17" "1" = doDisplay . DaySeventeen.PartOne.solution
solve "17" "2" = doDisplay . DaySeventeen.PartTwo.solution
solve "18" "1" = doDisplay . DayEighteen.PartOne.solution
solve "18" "2" = doDisplay . DayEighteen.PartTwo.solution
solve "19" "1" = doDisplay . DayNineteen.PartOne.solution
solve "19" "2" = doDisplay . DayNineteen.PartTwo.solution
solve "20" "1" = doDisplay . DayTwenty.PartOne.solution
solve "20" "2" = doDisplay . DayTwenty.PartTwo.solution
solve "21" "1" = doDisplay . DayTwentyOne.PartOne.solution
solve "21" "2" = doDisplay . DayTwentyOne.PartTwo.solution
solve "22" "1" = doDisplay . DayTwentyTwo.PartOne.solution
solve "22" "2" = doDisplay . DayTwentyTwo.PartTwo.solution
solve "23" "1" = doDisplay . DayTwentyThree.PartOne.solution
solve "23" "2" = doDisplay . DayTwentyThree.PartTwo.solution
solve "24" "1" = doDisplay . DayTwentyFour.PartOne.solution
solve "24" "2" = doDisplay . DayTwentyFour.PartTwo.solution
solve "25" "1" = doDisplay . DayTwentyFive.PartOne.solution
solve day part = const . putStrLn $ "No problem for day " ++ day ++ ", part " ++ part
