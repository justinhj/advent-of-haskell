import Test.HUnit
import Test5 (test5)

main :: IO ()
main = runTestTTAndExit $ TestList [test5]
