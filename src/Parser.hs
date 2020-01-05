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

data Token
    = TDot
    | TVar String
    | TOpeningBracket
    | TClosingBracket
    | TLambda
    deriving (Show, Eq)

tokensToTypes = Map.fromList
    [ ('\\', TLambda)
    , ('(' , TOpeningBracket)
    , (')' , TClosingBracket)
    , ('.' , TDot)
    ]

parse :: String -> Either String Expr
parse = parseFromTokens . tokenize

-- parseFromTokens :: [Token] -> Either String Expr
-- parseFromTokens []             = Left "Empty expression"
-- parseFromTokens [TVar var    ] = pure $ EVar var
-- parseFromTokens (TLambda : xs) = do
--     (remaining, args) <- extractArgs xs
--     case args of
--         [] -> Left "Empty argument list in lambda"
--         _  -> do
--             remExpr <- parseFromTokens remaining
--             pure $ newMultiArgumentLambda args remExpr
-- parseFromTokens xs = case last xs of
--     (TVar var) -> do
--         body <- parseFromTokens $ init xs
--         pure $ EApp body (EVar var)
--     TClosingBracket -> do
--         (left, right) <- splitOnMatchingLeftBracket (init xs)
--         case left of
--             [] -> parseFromTokens right
--             _  -> case right of
--                 [] -> Left "Empty expression in brackets"
--                 _  -> do
--                     parsedLeft  <- parseFromTokens left
--                     parsedRight <- parseFromTokens right
--                     pure $ EApp parsedLeft parsedRight
--     _ -> Left "Invalid expression"

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
        (left, right) <- splitOnMatchingLeftBracket (init xs)
        case left of
            [] -> parseFromTokens right
            _  -> case right of
                [] -> Left "Empty expression in brackets"
                _  -> do
                    parsedLeft  <- parseFromTokens left
                    parsedRight <- parseFromTokens right
                    pure $ EApp parsedLeft parsedRight
    _ -> Left "Invalid expression"


tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
    | Map.member x tokensToTypes = tokensToTypes Map.! x : tokenize xs
    | isSpace x                  = tokenize xs
    | otherwise                  = TVar [x] : tokenize xs

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
splitOnMatchingBracketUtil s acc [] | s > 0     = Left "Invalid brackets"
                                    | otherwise = pure (acc, [])
splitOnMatchingBracketUtil s acc (TOpeningBracket : TClosingBracket : xs) =
    Left "Empty expression in brackets"
splitOnMatchingBracketUtil s acc (TOpeningBracket : xs) =
    splitOnMatchingBracketUtil (s + 1) (acc ++ [TOpeningBracket]) xs
splitOnMatchingBracketUtil 1 acc (TClosingBracket : xs) = pure (acc, xs)
splitOnMatchingBracketUtil s acc (TClosingBracket : xs) =
    splitOnMatchingBracketUtil (s - 1) (acc ++ [TClosingBracket]) xs
splitOnMatchingBracketUtil s acc (x : xs) =
    splitOnMatchingBracketUtil s (acc ++ [x]) xs

splitOnMatchingLeftBracket :: [Token] -> Either String ([Token], [Token])
splitOnMatchingLeftBracket tokens = do
    (rightReversed, leftReversed) <- splitOnMatchingBracket
        (reverseTokens tokens)
    pure (reverseTokens leftReversed, reverseTokens rightReversed)

reverseTokens :: [Token] -> [Token]
reverseTokens tokens = map swapBrackets (reverse tokens)

swapBrackets :: Token -> Token
swapBrackets TOpeningBracket = TClosingBracket
swapBrackets TClosingBracket = TOpeningBracket
swapBrackets t               = t

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
