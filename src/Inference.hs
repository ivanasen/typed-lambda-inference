module Inference
    ( infer
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Data.Maybe                     ( fromMaybe )

import           InferenceTypes
import           Substitution

infer :: Expr -> Type
infer expr = snd $ evalState (inferUtil Map.empty expr) 0

inferUtil :: Context -> Expr -> TI (Substitution, Type)
inferUtil ctx (EVar var) = case Map.lookup var ctx of
    Nothing     -> error $ "Unbound variable: " ++ show var
    Just ty -> pure (Map.empty, ty)

inferUtil ctx (ELam arg body) = do
    tyArg <- newTyVar
    let tmpCtx = Map.insert arg tyArg ctx
    (s1, tyBody) <- inferUtil tmpCtx body
    pure (s1, applySubstToType s1 tyArg :-> tyBody)

inferUtil ctx (EApp fun arg) = do
    tyRes       <- newTyVar
    (s1, tyFun) <- inferUtil ctx fun
    (s2, tyArg) <- inferUtil (applySubstToContext s1 ctx) arg
    s3          <- unify (applySubstToType s2 tyFun) (tyArg :-> tyRes)
    let subst = composeSubst s3 $ composeSubst s2 s1
    pure (subst, applySubstToType s3 tyRes)

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    pure (TVar $ show s)

unify :: Type -> Type -> TI Substitution
unify (arg1 :-> res1) (arg2 :-> res2) = do
    s1 <- unify arg1 arg2
    s2 <- unify (applySubstToType s1 res1) (applySubstToType s1 res2)
    pure (composeSubst s1 s2)
unify (TVar var) t          = varBind var t
unify t          (TVar var) = varBind var t


varBind :: String -> Type -> TI Substitution
varBind var ty | ty == TVar var                   = pure Map.empty
               | Set.member var (freeTypeVars ty) = error "occurs check failed"
               | otherwise                        = pure (Map.singleton var ty)

freeTypeVars :: Type -> Set String
freeTypeVars (TVar var   ) = Set.singleton var
freeTypeVars (arg :-> res) = Set.union (freeTypeVars arg) (freeTypeVars res)
