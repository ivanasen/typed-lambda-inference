module ExprParser
    ( tokenize
    )
where

import           InferenceLib                   ( Expr(EVar, EApp, ELam), newMultiArgumentLam )
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
parseFromTokens [] = Nothing
parseFromTokens [TVar var] = pure $ EVar var
parseFromTokens (TLambda:xs) = do
    (remaining, args) <- extractArgs xs
    remExpr <- parseFromTokens remaining
    pure $ newMultiArgumentLam args remExpr
parseFromTokens (TVar fun : xs) = do
    body <- parseFromTokens xs
    pure $ EApp (EVar fun) body
parseFromTokens _ = Nothing

extractArgs :: [Token] -> Maybe ([Token], [String])
extractArgs (TDot : xs) = pure (xs, [])
extractArgs (TVar var : xs) = do
    (remaining, args) <- extractArgs xs
    pure (remaining, var:args)
extractArgs _ = Nothing

parse :: String -> Maybe Expr
parse = parseFromTokens . tokenize
    

-- matchBrackets :: [Token] -> ([Int])

-- parseFromTokens (TOpeningBracket:xs) = 
-- parseFromTokens [TLambda:args:TDot:body] = Just $ ELam (parseFromTokens args) (parseFromTokens body)
-- parseFromTokens [TOpeningBracket : fun : TClosingBracket : arg] = Just $ EApp (parseFromTokens fun) (parseFromTokens arg)
-- parseFromTokens [TOpeningBracket:tokens:TClosingBracket] = parseFromTokens tokens
-- parseFromTokens ((TVar fun):(TVar arg):[]) = Just $ EApp (EVar fun) (EVar arg)
-- parse _ = Nothing

-- parseExpr :: String -> Maybe Expr
-- parseExpr = parseFromTokens . tokenize
