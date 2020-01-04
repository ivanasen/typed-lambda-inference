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

import           Data.Either                    ( Either )

parseAndInfer :: String -> Either String (Expr, Type)
parseAndInfer s = do
    expr    <- parse s
    infered <- infer expr
    pure (expr, infered)

outputPrefix :: String
outputPrefix = "> "

runClient :: IO ()
runClient = forever $ do
    term <- getLine
    case parseAndInfer term of
        Left err -> putStrLn $ outputPrefix ++ "Error: " ++ err
        Right (expr, t) ->
            putStrLn $ outputPrefix ++ show expr ++ ": " ++ show t
    putStrLn ""
