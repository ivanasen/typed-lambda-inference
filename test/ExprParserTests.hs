module ExprParserTests
    ( testList
    )
where

import           Test.HUnit

import           InferenceTypes
import           Inference
import           ExprParser

import           TestUtils                      ( assertError )

testList = TestList [positiveTests, negativeTests]

positiveTests =
    TestLabel "parse_WhenPassedValidLambdaExpression_ParsesCorrectly" $ TestList
        [ TestCase (assertEqual "single variable" (pure $ EVar "x") $ parse "x")
        , TestCase
            ( assertEqual "single application"
                          (pure $ EApp (EVar "x") (EVar "y"))
            $ parse "x y"
            )
        , TestCase
            ( assertEqual
                    "muliplve applications"
                    (pure
                        (EApp
                            (EApp
                                (EApp
                                    (EApp (EApp (EVar "x") (EVar "y"))
                                          (EVar "z")
                                    )
                                    (EVar "u")
                                )
                                (EVar "v")
                            )
                            (EVar "w")
                        )
                    )
            $ parse "x y z u v w"
            )
        , TestCase
            ( assertEqual "simple case" (pure $ ELam "x" $ EVar "x")
            $ parse "\\x.x"
            )
        , TestCase
            ( assertEqual
                    "simple case"
                    (pure $ ELam "x" $ ELam "y" $ EApp (EVar "x") (EVar "y"))
            $ parse "\\xy.xy"
            )
        , TestCase
            ( assertEqual
                    "application is left-associative"
                    (pure $ ELam "x" $ ELam "y" $ ELam "z" $ EApp
                        (EApp (EVar "x") (EVar "y"))
                        (EVar "z")
                    )
            $ parse "\\xyz.xyz"
            )
        , TestCase
            ( assertEqual
                    "adding brackets breaks left-associativity"
                    (pure $ ELam "x" $ ELam "y" $ ELam "z" $ EApp
                        (EVar "x")
                        (EApp (EVar "y") (EVar "z"))
                    )
            $ parse "\\xyz.x(yz)"
            )
        , TestCase
            ( assertEqual "adding more brackets works"
                          (pure $ ELam "x" $ EVar "x")
            $ parse "\\x.(((((((((x)))))))))"
            )
        , TestCase
            ( assertEqual
                    "adding more brackets works"
                    (pure $ ELam "x" $ ELam "y" $ EApp (EVar "x") (EVar "y"))
            $ parse
                  "\\xy.((((((((((((((((((((x))))))))))))(((((((y)))))))))))))))"
            )
        , TestCase
            ( assertEqual
                    "having same variable doesn't affect the expression"
                    (pure $ ELam "x" $ ELam "x" $ EApp (EVar "x") (EVar "x"))
            $ parse "\\xx.xx"
            )
        , TestCase
            ( assertEqual
                    "multiple applications with lambdas"
                    (pure $ EApp
                        (EApp
                            (ELam "x" $ ELam "y" $ EApp (EVar "x") (EVar "y"))
                            (ELam "x" $ EVar "x")
                        )
                        (ELam "y" $ EVar "y")
                    )
            $ parse "(\\xy.xy) (\\x.x) (\\y.y)"
            )
        ]


negativeTests =
    TestLabel "parse_WhenPassedInvalidLambdaExpression_ReturnsError" $ TestList
        [ TestCase (assertError "empty string" (parse ""))
        , TestCase (assertError "empty lambda body" (parse "\\xyz."))
        , TestCase
            (assertError "brackets in argument list" (parse "\\(x)yz.zyx"))
        , TestCase (assertError "empty argument list" (parse "\\.zyx"))
        , TestCase (assertError "empty body" (parse "\\xyz."))
        , TestCase
            (assertError "empty body in application" (parse "(\\xyz.) x y z"))
        , TestCase
            (assertError "using reserved symbols for variables" (parse "."))
        , TestCase
            (assertError "using reserved symbols for variables" (parse "("))
        , TestCase
            (assertError "using reserved symbols for variables" (parse ")"))
        , TestCase
            (assertError "using reserved symbols for variables"
                         (parse "(\\x.x) .")
            )
        , TestCase
            (assertError
                "random string"
                (parse "eoihfoirehgiuregrehg\\dr..vrr\\vre(cdf()())r")
            )
        , TestCase (assertError "unmatched brackets" (parse "\\xyz.(x(y)z"))
        , TestCase
            (assertError "unmatched brackets"
                         (parse "(\\xyz.xy(z)) (\\x.(x) (\\y.y)")
            )
        , TestCase (assertError "empty brackets" (parse "\\xyz.()xyz"))
        , TestCase (assertError "empty brackets" (parse "\\xyz.((x()y()z))"))
        , TestCase
            (assertError
                "lambdas should be in brackets when there is an application"
                (parse "(\\x.x) \\x.x")
            )
        , TestCase (assertError "empty string" (parse ""))
        ]

