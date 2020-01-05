module Parser
    ( tokenize
    , parse
    , Token(TDot, TVar, TOpeningBracket, TClosingBracket, TLambda)
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Char                      ( isSpace )

import           InferenceTypes                 ( Expr(..)
                                                , newMultiArgumentLambda
                                                )
import           ParserTypes

parse :: String -> Either String Expr
parse = parseFromTokens . tokenize

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
    | Map.member x tokensToTypes = tokensToTypes Map.! x : tokenize xs
    | isSpace x                  = tokenize xs
    | otherwise                  = TVar [x] : tokenize xs

parseFromTokens :: [Token] -> Either String Expr
parseFromTokens []             = Left "Empty expression"
parseFromTokens [TVar var    ] = pure $ EVar var
parseFromTokens (TLambda : xs) = do
    (remaining, args) <- extractArgs xs
    case args of
        [] -> Left "Empty argument list in lambda"
        _  -> do
            remExpr <- parseFromTokens remaining
            pure $ newMultiArgumentLambda args remExpr
parseFromTokens xs = case last xs of
    (TVar var) -> do
        (left, right) <- splitOnLeftMostRightLambda xs
        case left of
            [] -> do
                body <- parseFromTokens $ init xs
                pure $ EApp body (EVar var)
            _ -> do
                rightParsed <- parseFromTokens right
                leftParsed  <- parseFromTokens left
                pure $ EApp leftParsed rightParsed
    TClosingBracket -> do
        (left, right) <- splitOnMatchingBracket (init xs)
        case left of
            [] -> parseFromTokens right
            _  -> case right of
                [] -> Left "Empty expression in brackets"
                _  -> do
                    parsedLeft  <- parseFromTokens left
                    parsedRight <- parseFromTokens right
                    pure $ EApp parsedLeft parsedRight
    _ -> Left "Invalid expression"

-- | extractArgs extracts all variable tokens
-- until a "." is encountered
-- or if something else is encountered returns an error
extractArgs :: [Token] -> Either String ([Token], [String])
extractArgs (TDot     : xs) = pure (xs, [])
extractArgs (TVar var : xs) = do
    (remaining, args) <- extractArgs xs
    pure (remaining, var : args)
extractArgs _ = Left "Only variables are allowed in argument list."

splitOnMatchingBracket :: [Token] -> Either String ([Token], [Token])
splitOnMatchingBracket = splitOnMatchingBracketUtil 1 []

splitOnMatchingBracketUtil
    :: Int -> [Token] -> [Token] -> Either String ([Token], [Token])
splitOnMatchingBracketUtil s acc tokens
    | null tokens && s > 0 = Left "Invalid brackets"
    | null tokens = pure ([], acc)
    | lt == TClosingBracket = splitOnMatchingBracketUtil (s + 1)
                                                         (lt : acc)
                                                         (init tokens)
    | lt == TOpeningBracket && s == 1 = pure (init tokens, acc)
    | lt == TOpeningBracket = splitOnMatchingBracketUtil (s - 1)
                                                         (lt : acc)
                                                         (init tokens)
    | otherwise = splitOnMatchingBracketUtil s (lt : acc) (init tokens)
    where lt = last tokens

splitOnLeftMostRightLambda :: [Token] -> Either String ([Token], [Token])
splitOnLeftMostRightLambda = splitOnLeftMostRightLambdaUtil 0 []

splitOnLeftMostRightLambdaUtil
    :: Int -> [Token] -> [Token] -> Either String ([Token], [Token])
splitOnLeftMostRightLambdaUtil s acc tokens
    | null tokens = pure (tokens, acc)
    | lt == TLambda && s == 0 = pure (init tokens, lt : acc)
    | lt == TClosingBracket = splitOnLeftMostRightLambdaUtil (s + 1)
                                                             (lt : acc)
                                                             (init tokens)
    | lt == TOpeningBracket && s == 0 = Left "Invalid brackets"
    | lt == TOpeningBracket = splitOnLeftMostRightLambdaUtil (s - 1)
                                                             (lt : acc)
                                                             (init tokens)
    | otherwise = splitOnLeftMostRightLambdaUtil s (lt : acc) (init tokens)
    where lt = last tokens
