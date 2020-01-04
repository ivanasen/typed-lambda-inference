import           Test.HUnit

import qualified InferenceTests

testList = TestList [InferenceTests.testList]

main :: IO ()
main = do
    runTestTT testList
    return()
