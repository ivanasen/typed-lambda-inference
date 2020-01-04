module Substitution
    ( Substitution
    , applySubstToType
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

applySubstToContext :: Substitution -> Context -> Context
applySubstToContext subst = Map.map (applySubstToType subst)

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union (Map.map (applySubstToType s1) s2) s1

