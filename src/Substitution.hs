module Substitution
    ( Substitution
    , applySubstToType
    , applySubstToScheme
    , applySubstToContext
    , composeSubst
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )

import           InferenceTypes

type Substitution = Map String Type

applySubstToType :: Substitution -> Type -> Type
applySubstToType subst ty = case ty of
    TVar var    -> fromMaybe (TVar var) (Map.lookup var subst)
    arg :-> res -> applySubstToType subst arg :-> applySubstToType subst res

applySubstToScheme :: Substitution -> Scheme -> Scheme
applySubstToScheme subst (Scheme vars t) =
    Scheme vars (applySubstToType (foldr Map.delete subst vars) t)

applySubstToContext :: Substitution -> Context -> Context
applySubstToContext subst = Map.map (applySubstToScheme subst)

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union (Map.map (applySubstToType s1) s2) s1

