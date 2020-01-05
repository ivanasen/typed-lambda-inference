module InferenceClient
    ( runClient
    )
where

import           Control.Monad                  ( forever )

import           InferenceTypes                 ( Expr
                                                , Type
                                                )
import           Inference                      ( inferType )
import           Parser                     ( parse )

import           Data.Either                    ( Either )

parseAndInfer :: String -> Either String (Expr, Type)
parseAndInfer s = do
    expr    <- parse s
    inferred <- inferType expr
    pure (expr, inferred)

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
