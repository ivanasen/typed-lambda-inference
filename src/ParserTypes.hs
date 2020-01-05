module ParserTypes
    ( Token(..)
    , tokensToTypes
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

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
