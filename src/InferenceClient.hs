module InferenceClient
    ( runClient
    )
where

import           Control.Monad                  ( forever )

import           InferenceLib                   ( Expr
                                                , infer
                                                , prettyShow
                                                , Type
                                                )
import           ExprParser                     ( parse )

parseAndInfer :: String -> Maybe (Expr, Type)
parseAndInfer s = do
    expr <- parse s
    pure (expr, infer expr)

runClient :: IO ()
runClient = forever $ do
    putStrLn "Type in a lambda term:"
    term <- getLine
    case parseAndInfer term of
        Nothing -> putStrLn "Invalid lambda term."
        Just (expr, t)  -> putStrLn $ show expr ++ ": " ++ prettyShow t
