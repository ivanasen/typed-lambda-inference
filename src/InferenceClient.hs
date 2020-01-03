module InferenceClient
    ( runClient
    )
where

import           Control.Monad                  ( forever )

import           InferenceLib                   ( infer
                                                , prettyShow
                                                , Type
                                                )
import           ExprParser                     ( parse )

parseAndInfer :: String -> Maybe Type
parseAndInfer s = do
    expr <- parse s
    pure $ infer expr

runClient :: IO ()
runClient = forever $ do
    putStrLn "Type in a lambda term:"
    term <- getLine
    case parseAndInfer term of
        Nothing -> putStrLn "Invalid lambda term."
        Just t  -> putStrLn $ "Term is of type: " ++ prettyShow t
