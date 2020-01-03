module InferenceTypes
    ( TI
    , Expr(..)
    , Type(..)
    , Scheme(..)
    , Context
    , newMultiArgumentLam
    )
where

import           Control.Monad.State
import           Data.Map                       ( Map )


type TI a = State Int a

data Expr
    = EVar String
    | EApp Expr Expr
    | ELam String Expr
    deriving (Eq)

instance Show Expr where
    show (EVar var                ) = var
    show (EApp fun@(ELam _ _) arg ) = "(" ++ show fun ++ ") " ++ show arg
    show (EApp fun            arg ) = show fun ++ " " ++ show arg
    show (ELam arg            body) = "λ" ++ arg ++ "." ++ show body


infixr 5 :->
data Type
    = TVar String
    | Type :-> Type
    deriving (Eq)

instance Show Type where
    show (TVar var   ) = var
    show (arg :-> res) = "(" ++ show arg ++ " -> " ++ show res ++ ")"

data Scheme = Scheme [String] Type
    deriving (Eq)

type Context = Map String Scheme

newMultiArgumentLam :: [String] -> Expr -> Expr
newMultiArgumentLam [arg] body = ELam arg body
newMultiArgumentLam (arg : otherArgs) body =
    ELam arg $ newMultiArgumentLam otherArgs body
