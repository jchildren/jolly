module Jolly.Types.Env
  ( TypeEnv(..)
  , emptyTypeEnv
  , remove
  , extend
  , lookupType
  ) where

import qualified Data.List          as List
import qualified Data.Map.Strict    as Map

import           Jolly.Syntax.Types
import           Jolly.Types.System

newtype TypeEnv = TypeEnv
  { types :: Map.Map Name Scheme
  } deriving (Eq, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: TypeEnv -> [(Name, Scheme)] -> TypeEnv
extends env xs = env {types = Map.union (Map.fromList xs) (types env)}

lookupType :: Name -> TypeEnv -> Maybe Scheme
lookupType key (TypeEnv tys) = Map.lookup key tys

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [TypeEnv] -> TypeEnv
mergeEnvs = List.foldl' merge emptyTypeEnv

singleton :: Name -> Scheme -> TypeEnv
singleton x y = TypeEnv (Map.singleton x y)

keys :: TypeEnv -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> TypeEnv
fromList xs = TypeEnv (Map.fromList xs)

toList :: TypeEnv -> [(Name, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Monoid TypeEnv where
  mempty = emptyTypeEnv
  mappend = merge
