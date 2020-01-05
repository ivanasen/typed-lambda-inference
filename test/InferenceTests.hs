module InferenceTests
    ( testList
    )
where

import           Test.HUnit

import           InferenceTypes                 ( Expr(..)
                                                , Type(..)
                                                )
import           Inference                      ( infer
                                                , showPrettyVar
                                                )

testList =
    TestList [positiveInferTests, negativeInferTests, showPrettyVarTests]

isError :: Either a b -> Bool
isError (Left _) = True
isError _        = False

positiveInferTests =
    TestLabel "infer_WhenPassedValidTypeExpression_ReturnsCorrectType"
        $ TestList
              [ TestCase
                  (assertEqual "\\x.x: A -> A"
                               (pure $ TVar "A" :-> TVar "A")
                               (infer (ELam "x" $ EVar "x"))
                  )
              , TestCase
                  (assertEqual
                      "\\xyz.xyz: (B -> (C -> D)) -> B -> C -> D)"
                      (   pure
                      $   (TVar "B" :-> (TVar "C" :-> TVar "D"))
                      :-> TVar "B"
                      :-> TVar "C"
                      :-> TVar "D"
                      )
                      (infer
                          (ELam "x" $ ELam "y" $ ELam "z" $ EApp
                              (EApp (EVar "x") (EVar "y"))
                              (EVar "z")
                          )
                      )
                  )
              , TestCase
                  (assertEqual
                      "\\fx.fx: (B -> C) -> B -> C"
                      (pure $ (TVar "B" :-> TVar "C") :-> TVar "B" :-> TVar "C")
                      (infer $ ELam "f" $ ELam "x" $ EApp (EVar "f") (EVar "x"))
                  )
              ]

negativeInferTests =
    TestLabel "infer_WhenPassedInvalidTypeExpression_ReturnsError" $ TestList
        [ TestCase
            (assertBool
                "\\x.xx is a recursive expresion which isn't supported by Simply typed lambda calculus"
                (isError $ infer $ ELam "x" $ EApp (EVar "x") (EVar "x"))
            )
        , TestCase
            (assertBool
                "\\xy.xyx is a recursive expresion which isn't supported by Simply typed lambda calculus"
                ( isError
                $ infer
                      (ELam "x" $ ELam "y" $ EApp
                          (EApp (EVar "x") (EVar "y"))
                          (EVar "x")
                      )
                )
            )
        , TestCase
            (assertBool "\\x.y has an unbound variable"
                        (isError $ infer $ ELam "x" $ EVar "y")
            )
        , TestCase
            (assertBool
                "\\xy.xa has an unbound variable 'a'"
                (isError $ infer $ ELam "x" $ ELam "y" $ EApp (EVar "x")
                                                              (EVar "a")
                )
            )
        ]

showPrettyVarTests =
    TestLabel "showPrettyVar_WhenPassedANumber_ReturnsCorrectVariableName"
        $ TestList
              [ TestCase
                  (assertEqual
                      "if number is less than 0 then the result is \"\""
                      ""
                      (showPrettyVar (-23))
                  )
              , TestCase (assertEqual "first letter is A" "A" (showPrettyVar 0))
              , TestCase (assertEqual "" "Z" (showPrettyVar 25))
              , TestCase (assertEqual "" "AB" (showPrettyVar 26))
              , TestCase (assertEqual "" "BB" (showPrettyVar 27))
              , TestCase (assertEqual "" "CB" (showPrettyVar 28))
              , TestCase (assertEqual "" "HN" (showPrettyVar 345))
              ]
