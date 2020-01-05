module TestUtils
    ( assertError
    )
where

import Test.HUnit

isError :: Either a b -> Bool
isError (Left _) = True
isError _        = False

assertError :: String -> Either a b -> Assertion
assertError label tested = assertBool label (isError tested)

