module Inference
    ( inferType
    , showPrettyVar
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Monad.State

import           InferenceTypes
import           Substitution

initialVariableState = 0

-- | inferType is the implementation of the Hindley-Milner type inference algorithm.
-- It takes an Expression and returns Either an error or a type
-- corresponding to that expression, according to the Simply typed lambda calculus.
-- inferType :: Expr -> Either String Type
-- inferType expr =
--     evalState (inferTypeUtil Map.empty expr) initialVariableState
--         >>= (\(s, ty) -> pure ty)
inferType :: Expr -> Either String Type
inferType expr =
    evalStateT (inferTypeUtil Map.empty expr) initialVariableState
        >>= (\(subst, ty) -> pure ty)


-- inferTypeUtil
--     :: Context -> Expr -> VarCountState (Either String (Substitution, Type))
inferTypeUtil :: Context -> Expr -> VarCountState (Substitution, Type)


inferTypeUtil ctx (EVar var) = case Map.lookup var ctx of
    Nothing -> lift $ Left $ "Unbound variable: " ++ show var
    Just ty -> pure (Map.empty, ty)

inferTypeUtil ctx (ELam arg body) = do
    tyArg <- newTyVar
    let tmpCtx = Map.insert arg tyArg ctx
    (s1, tyBody) <- inferTypeUtil tmpCtx body
    pure (s1, applySubstToType s1 tyArg :-> tyBody)

inferTypeUtil ctx (EApp fun arg) = do
    tyRes       <- newTyVar
    (s1, tyFun) <- inferTypeUtil ctx fun
    (s2, tyArg) <- inferTypeUtil (applySubstToContext s1 ctx) arg
    s3          <- unify (applySubstToType s2 tyFun) (tyArg :-> tyRes)
    let subst = composeSubst s3 $ composeSubst s2 s1
    pure (subst, applySubstToType s3 tyRes)

-- | unify takes two types t1 and t2, tries to unify them
-- and returns the substitution which needs to be applied to make the equal.
-- unify is also commutative
-- Examples (a and b are types variables):
--     a U b == a
--     a U (b -> c) == (b -> c)
--     (a -> b) U (c -> d) == (a -> b)
--     (a -> b) U (c -> b) == (b -> b)
--     a U (a -> a) == Error, because simply typed lambda calculus
--         doesn't support recursive types.
unify :: Type -> Type -> VarCountState Substitution
unify (arg1 :-> res1) (arg2 :-> res2) = do
    s1 <- unify arg1 arg2
    s2 <- unify (applySubstToType s1 res1) (applySubstToType s1 res2)
    pure (composeSubst s1 s2)
unify (TVar var) t          = varBind var t
unify t          (TVar var) = varBind var t

newTyVar :: VarCountState Type
newTyVar = do
    s <- get
    put (s + 1)
    pure (TVar $ showPrettyVar s)

varBind :: String -> Type -> VarCountState Substitution
varBind var ty
    | ty == TVar var = pure Map.empty
    | Set.member var (freeTypeVars ty) = lift
    $ Left "Recursive types are not supported"
    | otherwise = pure (Map.singleton var ty)

freeTypeVars :: Type -> Set String
freeTypeVars (TVar var   ) = Set.singleton var
freeTypeVars (arg :-> res) = Set.union (freeTypeVars arg) (freeTypeVars res)

showPrettyVar :: Int -> String
showPrettyVar n
    | n < 0     = ""
    | n < len   = [alphabet !! n]
    | otherwise = alphabet !! (n `rem` len) : showPrettyVar (n `div` len)
  where
    alphabet = ['A', 'B' .. 'Z']
    len      = length alphabet
