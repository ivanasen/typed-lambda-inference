module ExprParser
    ( tokenize
    , parse
    , Token(TDot, TVar, TOpeningBracket, TClosingBracket, TLambda)
    , splitOnRightMostClosingBracket
    )
where

import           InferenceLib                   ( Expr(EVar, EApp, ELam)
                                                , newMultiArgumentLam
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Char                      ( isSpace )

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

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
    | Map.member x tokensToTypes = tokensToTypes Map.! x : tokenize xs
    | isSpace x                  = tokenize xs
    | otherwise                  = TVar [x] : tokenize xs

parseFromTokens :: [Token] -> Maybe Expr
parseFromTokens []             = Nothing
parseFromTokens [TVar var    ] = pure $ EVar var
parseFromTokens (TLambda : xs) = do
    (remaining, args) <- extractArgs xs
    remExpr           <- parseFromTokens remaining
    pure $ newMultiArgumentLam args remExpr
parseFromTokens (TVar fun : xs) = do
    body <- parseFromTokens xs
    pure $ EApp (EVar fun) body
parseFromTokens (TOpeningBracket : xs) = do
    let (left, right) = splitOnRightMostClosingBracket xs
    let leftNoBracket = init left
    if null leftNoBracket
        then Nothing
        else if null right
            then parseFromTokens leftNoBracket
            else do
                parsedLeft  <- parseFromTokens leftNoBracket
                parsedRight <- parseFromTokens right
                pure $ EApp parsedLeft parsedRight
parseFromTokens _ = Nothing

extractArgs :: [Token] -> Maybe ([Token], [String])
extractArgs (TDot     : xs) = pure (xs, [])
extractArgs (TVar var : xs) = do
    (remaining, args) <- extractArgs xs
    pure (remaining, var : args)
extractArgs _ = Nothing

parse :: String -> Maybe Expr
parse = parseFromTokens . tokenize


areValidBrackets :: [Token] -> Bool
areValidBrackets = areValidBracketsUtil 0

areValidBracketsUtil :: Int -> [Token] -> Bool
areValidBracketsUtil _ []                     = True
areValidBracketsUtil s (TOpeningBracket : xs) = areValidBracketsUtil (s + 1) xs
areValidBracketsUtil s (TClosingBracket : xs)
    | s == 0    = False
    | otherwise = areValidBracketsUtil (s - 1) xs
areValidBracketsUtil s (_ : tokens) = areValidBracketsUtil s tokens

-- parseFromTokens (TOpeningBracket:xs) = 
-- parseFromTokens [TLambda:args:TDot:body] = Just $ ELam (parseFromTokens args) (parseFromTokens body)
-- parseFromTokens [TOpeningBracket : fun : TClosingBracket : arg] = Just $ EApp (parseFromTokens fun) (parseFromTokens arg)
-- parseFromTokens [TOpeningBracket:tokens:TClosingBracket] = parseFromTokens tokens
-- parseFromTokens ((TVar fun):(TVar arg):[]) = Just $ EApp (EVar fun) (EVar arg)
-- parse _ = Nothing

-- parseExpr :: String -> Maybe Expr
-- parseExpr = parseFromTokens . tokenize

splitOnRightMostClosingBracket :: [Token] -> ([Token], [Token])
splitOnRightMostClosingBracket = foldr
    (\currentToken (leftPart, rightPart) ->
        if not (null leftPart) || currentToken == TClosingBracket
            then (currentToken : leftPart, rightPart)
            else (leftPart, currentToken : rightPart)
    )
    ([], [])
