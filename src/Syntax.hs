{-# LANGUAGE RankNTypes #-}

module Syntax
  ( Name
  , Expr(..)
  , BinOp(..)
  , Lit(..)
  ) where

type Name = String

data Expr
  = Var Name
  | Lam Name
        Expr
  | App Expr
        Expr
  | Lit Lit
  | Op BinOp
       Expr
       Expr
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Eql
  deriving (Eq, Ord, Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)
