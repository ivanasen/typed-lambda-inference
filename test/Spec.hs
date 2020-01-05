import           Test.HUnit

import qualified InferenceTests
import qualified ParserTests

testList = TestList [InferenceTests.testList, ParserTests.testList]

main :: IO ()
main = do
    runTestTT testList
    return ()
