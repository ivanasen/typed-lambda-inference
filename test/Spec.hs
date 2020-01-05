import           Test.HUnit

import qualified InferenceTests
import qualified ExprParserTests

testList = TestList [InferenceTests.testList, ExprParserTests.testList]

main :: IO ()
main = do
    runTestTT testList
    return ()
