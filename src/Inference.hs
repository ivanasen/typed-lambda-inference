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
import           Data.Maybe                     ( fromMaybe )

import           InferenceTypes
import           Substitution

initialVariableState = 0

-- | inferType is the implementation of the Hindley-Milner type inference algorithm.
-- It takes an Expression and returns Either an error or a type
-- corresponding to that expression, according to the Simply typed lambda calculus.
inferType :: Expr -> Either String Type
inferType expr =
    case evalState (inferTypeUtil Map.empty expr) initialVariableState of
        Left  err     -> Left err
        Right (_, ty) -> pure ty

inferTypeUtil
    :: Context -> Expr -> VarCountState (Either String (Substitution, Type))
inferTypeUtil ctx (EVar var) = case Map.lookup var ctx of
    Nothing -> pure $ Left $ "Unbound variable: " ++ show var
    Just ty -> pure $ pure (Map.empty, ty)

inferTypeUtil ctx (ELam arg body) = do
    tyArg <- newTyVar
    let tmpCtx = Map.insert arg tyArg ctx
    bodyRes <- inferTypeUtil tmpCtx body
    case bodyRes of
        Right (s1, tyBody) ->
            pure $ pure (s1, applySubstToType s1 tyArg :-> tyBody)
        Left err -> pure $ Left err

inferTypeUtil ctx (EApp fun arg) = do
    tyRes    <- newTyVar
    tyFunRes <- inferTypeUtil ctx fun
    case tyFunRes of
        Left  err         -> pure $ Left err
        Right (s1, tyFun) -> do
            tyArgRes <- inferTypeUtil (applySubstToContext s1 ctx) arg
            case tyArgRes of
                Left  err         -> pure $ Left err
                Right (s2, tyArg) -> do
                    s3Res <- unify (applySubstToType s2 tyFun) (tyArg :-> tyRes)
                    case s3Res of
                        Right s3 -> do
                            let subst = composeSubst s3 $ composeSubst s2 s1
                            pure $ pure (subst, applySubstToType s3 tyRes)
                        Left err -> pure $ Left err

-- | unify takes two types t1 and t2, tries to unify them
-- and returns the substitution which needs to be applied to make the equal.
-- unify is commutative
-- Examples:
--     a U b == a
--     a U (b -> c) == (b -> c)
--     (a -> b) U (c -> d) == (a -> b)
--     (a -> b) U (c -> b) == (b -> b)
--     a U (a -> a) == Error, because simply typed lambda calculus
--         doesn't support recursive types.
unify :: Type -> Type -> VarCountState (Either String Substitution)
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

newTyVar :: VarCountState Type
newTyVar = do
    s <- get
    put (s + 1)
    pure (TVar $ showPrettyVar s)

varBind :: String -> Type -> VarCountState (Either String Substitution)
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
