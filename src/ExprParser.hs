module ExprParser
    ( tokenize
    , parse
    , Token(TDot, TVar, TOpeningBracket, TClosingBracket, TLambda)
    , reverseAssociativity
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Char                      ( isSpace )

import           InferenceTypes                 ( Expr(..)
                                                , newMultiArgumentLam
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
    remExpr           <- parseFromTokens remaining
    pure $ newMultiArgumentLam args remExpr
parseFromTokens (TVar fun : xs) = do
    body <- parseFromTokens xs
    case body of
        app@(EApp bodyFun bodyArg) -> if head xs == TOpeningBracket
            then pure $ EApp funVar app
            else pure $ EApp (EApp funVar bodyFun) bodyArg
        varOrLam -> pure $ EApp funVar varOrLam
    where funVar = EVar fun

parseFromTokens (TOpeningBracket : xs) = do
    (left, right) <- splitOnMatchingBracket xs
    parsedLeft    <- parseFromTokens left
    if null right
        then pure parsedLeft
        else do
            parsedRight <- parseFromTokens right
            pure $ EApp parsedLeft parsedRight
parseFromTokens _ = Left "Invalid expression"

extractArgs :: [Token] -> Either String ([Token], [String])
extractArgs (TDot     : xs) = pure (xs, [])
extractArgs (TVar var : xs) = do
    (remaining, args) <- extractArgs xs
    pure (remaining, var : args)
extractArgs _ = Left "Only variables are allowed in argument list."

parse :: String -> Either String Expr
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

reverseAssociativity :: Expr -> Expr
reverseAssociativity app@(EApp fun@(EVar _) arg@(EVar _)) = app
reverseAssociativity (EApp fun@(EVar _) (EApp argFun argArg)) =
    EApp (EApp fun $ getLeftMost argFun) (reverseAssociativity argArg)
reverseAssociativity e = e

getLeftMost :: Expr -> Expr
getLeftMost (EApp fun arg) = getLeftMost fun
getLeftMost e              = e
