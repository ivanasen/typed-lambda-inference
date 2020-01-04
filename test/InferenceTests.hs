module InferenceTests
    ( testList
    )
where

import           Test.HUnit

import           InferenceTypes                 ( Expr(..)
                                                , Type(..)
                                                )
import           Inference                      ( infer )

testList = TestList [positiveInferTests, negativeInferTests]

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

isError :: Either a b -> Bool
isError (Left _) = True
isError _        = False
