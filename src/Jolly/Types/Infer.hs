{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Jolly.Types.Infer
  ( inferTop
  , Constraint
  , Subst(..)
  , TypeError(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.List              as List
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import           Jolly.Syntax.Types
import           Jolly.Types.Env
import           Jolly.Types.System

-- Infer contains the state and errors encountered
-- during the inference process
type Infer a = (ReaderT TypeEnv (StateT InferState (Except TypeError)) a)

newtype InferState = InferState
  { count :: Int
  }

initInfer :: InferState
initInfer = InferState {count = 0}

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

-- Substitution
newtype Subst =
  Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)           = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2)     = apply s t1 `TArr` apply s t2
  ftv TCon {}        = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
  = UnificationFail Type
                    Type
  | InfiniteType TVar
                 Type
  | UnboundVariable String
  | Ambiguous [Constraint]
  | UnificationMismatch [Type]
                        [Type]

runInfer ::
     TypeEnv
  -> Infer (Type, [Constraint])
  -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err    -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

constraintsExpr ::
     TypeEnv -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where sc = closeOver $ apply subst ty

closeOver :: Type -> Scheme
closeOver = normalize . generalize emptyTypeEnv

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> instantiate s

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

ops :: BinOp -> Type
ops Add = typeInt `TArr` (typeInt `TArr` typeInt)
ops Mul = typeInt `TArr` (typeInt `TArr` typeInt)
ops Sub = typeInt `TArr` (typeInt `TArr` typeInt)
ops Eql = typeInt `TArr` (typeInt `TArr` typeBool)

-- TODO: These case statements could be combined
infer :: Expr -> Infer (Type, [Constraint])
infer expr =
  case expr of
    Lit (LInt _) -> return (typeInt, [])
    Lit (LBool _) -> return (typeBool, [])
    Var x -> do
      t <- lookupEnv x
      return (t, [])
    Lam x e -> do
      tv <- fresh
      (t, c) <- inEnv (x, Forall [] tv) (infer e)
      return (tv `TArr` t, c)
    App e1 e2 -> do
      (t1, c1) <- infer e1
      (t2, c2) <- infer e2
      tv <- fresh
      return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])
    Let x e1 e2 -> do
      (t1, c1) <- infer e1
      env <- ask
      case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
          let sc = generalize (apply sub env) (apply sub t1)
          (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
          return (t2, c1 ++ c2)
    Fix e -> do
      (t, c) <- infer e
      tv <- fresh
      return (tv, c ++ [(tv `TArr` tv, t)])
    Op op lhs rhs -> do
      (t1, c1) <- infer lhs
      (t2, c2) <- infer rhs
      tv <- fresh
      let u1 = t1 `TArr` (t2 `TArr` tv)
          u2 = ops op
      return (tv, c1 ++ c2 ++ [(u1, u2)])
    If cond tr fl -> do
      (t1, c1) <- infer cond
      (t2, c2) <- infer tr
      (t3, c3) <- infer fl
      return (t2, c1 ++ c2 ++ c3 ++ [(t1, typeBool), (t2, t3)])

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) =
  case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (List.nub $ fv body) (map TV letters)
    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a) = TCon a
    normtype (TVar a) =
      case Prelude.lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"

-- Constraint Solver
emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) =
  Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where
    st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2
  | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2):cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Type -> Solve Subst
bind a t
  | t == TVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
