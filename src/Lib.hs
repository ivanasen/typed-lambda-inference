module Lib
    ( Type(Primitive, Function, Variable)
    , Expression(EVar, EFunc, ECall)
    , Context(Context)
    , someFunc
    , inferType
    , typeToString
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

data Type =
    Primitive { name :: String } |
    Function { src :: Type, dst :: Type } |
    Variable { varName :: Int }
    deriving (Show, Eq)

data Expression =
    EVar String |
    EFunc String Expression |
    ECall Expression Expression
    deriving (Show, Eq)

data Context = Context { nextId :: Int, env :: Map String Type }
    deriving (Show)

type Substitution = Map String Type

addVarToContext :: String -> Context -> Type -> Context
addVarToContext name (Context nextId env) var =
    Context (nextId + 1) (Map.insert name var env)

newTVariable :: Context -> Type
newTVariable ctx = Variable (nextId ctx)

applySubstitution :: Substitution -> Type -> Type
applySubstitution sub (Variable name)
    | Map.member (show name) sub = sub Map.! show name
    | otherwise                  = Variable name
applySubstitution sub (Function src dst) =
    Function (applySubstitution sub src) (applySubstitution sub dst)

inferTypeUtil :: Context -> Expression -> Maybe (Type, Substitution)

inferTypeUtil (Context _ env) (EVar varName)
    | Map.member varName env = Just (env Map.! varName, Map.empty)
    | otherwise              = Nothing

inferTypeUtil ctx (EFunc arg body) = do
    let newVariable = newTVariable ctx
    let newEnv      = addVarToContext arg ctx newVariable
    (bodyType, subst) <- inferTypeUtil newEnv body
    return (Function (applySubstitution subst newVariable) bodyType, subst)

-- inferTypeUtil ctx (ECall func arg) = do


inferType :: Expression -> Maybe Type
inferType expr = do
    (infered, subs) <- inferTypeUtil (Context 0 Map.empty) expr
    return infered

typeToString :: Maybe Type -> String
typeToString Nothing                 = "Type is Nothing"
typeToString (Just (Primitive name)) = name
typeToString (Just (Variable  name)) = show name
typeToString (Just (Function src dst)) =
    "(" ++ typeToString (Just src) ++ " -> " ++ typeToString (Just dst) ++ ")"

someFunc :: IO ()
someFunc = putStrLn "Hello Worlds"
