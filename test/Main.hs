import Test.HUnit
import Test4 (test4)
import Test5 (test5)

main :: IO ()
main = runTestTTAndExit $ TestList [test4, test5]
