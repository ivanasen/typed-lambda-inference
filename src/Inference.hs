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

infer :: Expr -> Either String Type
infer expr = case evalState (inferUtil Map.empty expr) 0 of
    Left  err     -> Left err
    Right (_, ty) -> pure ty

inferUtil :: Context -> Expr -> TI (Either String (Substitution, Type))
inferUtil ctx (EVar var) = case Map.lookup var ctx of
    Nothing -> pure $ Left $ "Unbound variable: " ++ show var
    Just ty -> pure $ pure (Map.empty, ty)

inferUtil ctx (ELam arg body) = do
    tyArg <- newTyVar
    let tmpCtx = Map.insert arg tyArg ctx
    bodyRes <- inferUtil tmpCtx body
    case bodyRes of
        Right (s1, tyBody) ->
            pure $ pure (s1, applySubstToType s1 tyArg :-> tyBody)
        Left err -> pure $ Left err

inferUtil ctx (EApp fun arg) = do
    tyRes    <- newTyVar
    tyFunRes <- inferUtil ctx fun
    case tyFunRes of
        Left  err         -> pure $ Left err
        Right (s1, tyFun) -> do
            tyArgRes <- inferUtil (applySubstToContext s1 ctx) arg
            case tyArgRes of
                Left  err         -> pure $ Left err
                Right (s2, tyArg) -> do
                    s3Res <- unify (applySubstToType s2 tyFun) (tyArg :-> tyRes)
                    case s3Res of
                        Right s3 -> do
                            let subst = composeSubst s3 $ composeSubst s2 s1
                            pure $ pure (subst, applySubstToType s3 tyRes)
                        Left err -> pure $ Left err

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    pure (TVar $ showPrettyVar s)

unify :: Type -> Type -> TI (Either String Substitution)
unify (arg1 :-> res1) (arg2 :-> res2) = do
    s1Res <- unify arg1 arg2
    case s1Res of
        Right s1 -> do
            s2Res <- unify (applySubstToType s1 res1) (applySubstToType s1 res2)
            case s2Res of
                Right s2 -> pure $ pure (composeSubst s1 s2)
                err      -> pure err
        err -> pure err
unify (TVar var) t          = varBind var t
unify t          (TVar var) = varBind var t


varBind :: String -> Type -> TI (Either String Substitution)
varBind var ty
    | ty == TVar var = pure $ pure Map.empty
    | Set.member var (freeTypeVars ty) = pure
    $ Left "Recursive types are not supported"
    | otherwise = pure $ pure (Map.singleton var ty)

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
