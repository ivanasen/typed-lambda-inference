module InferenceLib
    ( Expr(EVar, EApp, ELam)
    , Type(TVar, (:->))
    , infer
    , TI
    )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Data.Maybe                     ( fromMaybe )

type TI a = State Int a

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    pure (TVar ("u" ++ show s))

data Expr
    = EVar String
    | EApp Expr Expr
    | ELam String Expr
    deriving (Show, Eq)

infixr 5 :->
data Type
    = TVar String
    | Type :-> Type
    deriving (Show, Eq)

data Scheme = Scheme [String] Type
    deriving (Show, Eq)

type Context = Map String Scheme

type Substitution = Map String Type

emptySubst = Map.empty

inferUtil :: Context -> Expr -> TI (Substitution, Type)
inferUtil ctx (EVar var) = case Map.lookup var ctx of
    Nothing     -> error $ "Unbound variable: " ++ show var
    Just scheme -> do
        ty <- instantiate scheme
        pure (emptySubst, ty)

inferUtil ctx (ELam arg body) = do
    tyArg <- newTyVar
    let tmpCtx = Map.insert arg (Scheme [] tyArg) ctx
    (s1, tyBody) <- inferUtil tmpCtx body
    pure (s1, applySubstToType s1 tyArg :-> tyBody)

inferUtil ctx (EApp fun arg) = do
    tyRes       <- newTyVar
    (s1, tyFun) <- inferUtil ctx fun
    (s2, tyArg) <- inferUtil (applySubstToContext s1 ctx) arg
    s3          <- unify (applySubstToType s2 tyFun) (tyArg :-> tyRes)
    pure (s3, tyRes)

-- infer :: Expr -> TI Type
-- infer expr = do
--     (subst, ty) <-inferUtil Map.empty expr
--     pure ty

infer :: Expr -> Type
infer expr = let s = inferUtil Map.empty expr in snd . fst $ runState s 0

applySubstToType :: Substitution -> Type -> Type
applySubstToType subst ty = case ty of
    TVar var -> fromMaybe (TVar var) (Map.lookup var subst)
    arg :-> res ->
        applySubstToType subst arg :-> applySubstToType subst res

applySubstToScheme :: Substitution -> Scheme -> Scheme
applySubstToScheme subst (Scheme vars t) =
    Scheme vars (applySubstToType (foldr Map.delete subst vars) t)

applySubstToContext :: Substitution -> Context -> Context
applySubstToContext subst = Map.map (applySubstToScheme subst)

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union (Map.map (applySubstToType s1) s2) s1

instantiate :: Scheme -> TI Type
instantiate (Scheme vars ty) = do
    newVars <- traverse (const newTyVar) vars
    let subst = Map.fromList (zip vars newVars)
    pure (applySubstToType subst ty)

varBind :: String -> Type -> TI Substitution
varBind var ty | ty == TVar var                   = pure emptySubst
               | Set.member var (freeTypeVars ty) = error "occurs check failed"
               | otherwise                        = pure (Map.singleton var ty)

unify :: Type -> Type -> TI Substitution
unify (arg1 :-> res1) (arg2 :-> res2) = do
    s1 <- unify arg1 arg2
    s2 <- unify (applySubstToType s1 res1) (applySubstToType s1 res2)
    pure (composeSubst s1 s2)
unify (TVar var) t          = varBind var t
unify t          (TVar var) = varBind var t
-- unify t1 t2 = error $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

freeTypeVars :: Type -> Set String
freeTypeVars (TVar var    ) = Set.singleton var
freeTypeVars (arg :-> res) = Set.union (freeTypeVars arg) (freeTypeVars res)
