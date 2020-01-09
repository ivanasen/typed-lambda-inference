module InferenceTypes
    ( VarCountState
    , Expr(..)
    , Type(..)
    , Context
    , newMultiArgumentLambda
    )
where

import           Control.Monad.State
import           Data.Map                       ( Map )

type VarCountState a = StateT Int (Either String) a

data Expr
    = EVar String
    | EApp Expr Expr
    | ELam String Expr
    deriving (Eq)

instance Show Expr where
    show (EVar var     ) = var
    show (EApp fun arg ) = "(" ++ show fun ++ " " ++ show arg ++ ")"
    show (ELam arg body) = "(λ" ++ arg ++ "." ++ show body ++ ")"

newMultiArgumentLambda :: [String] -> Expr -> Expr
newMultiArgumentLambda [arg] body = ELam arg body
newMultiArgumentLambda (arg : otherArgs) body =
    ELam arg $ newMultiArgumentLambda otherArgs body


infixr 5 :->
data Type
    = TVar String
    | Type :-> Type
    deriving (Eq)

instance Show Type where
    show (TVar var   ) = var
    show (arg :-> res) = "(" ++ show arg ++ " -> " ++ show res ++ ")"


type Context = Map String Type
