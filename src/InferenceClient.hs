module InferenceClient
    ( runClient
    )
where

import           Control.Monad                  ( forever )

import           InferenceTypes                 ( Expr
                                                , Type
                                                )
import           Inference                      ( infer )
import           ExprParser                     ( parse )

parseAndInfer :: String -> Maybe (Expr, Type)
parseAndInfer s = do
    expr <- parse s
    pure (expr, infer expr)

runClient :: IO ()
runClient = forever $ do
    term <- getLine
    case parseAndInfer term of
        Nothing        -> putStrLn "Invalid lambda expression"
        Just (expr, t) -> putStrLn $ show expr ++ ": " ++ show t
