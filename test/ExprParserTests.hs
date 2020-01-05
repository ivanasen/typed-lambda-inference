module ExprParserTests
    ( testList
    )
where

import           Test.HUnit

import           InferenceTypes
import           Inference
import           ExprParser

testList = TestList [positiveTests]

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
            $ parse "x y z w u v"
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
        ]
