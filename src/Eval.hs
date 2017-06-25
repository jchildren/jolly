module Eval
  ( Value(..)
  , Env
  , emptyEnv
  , eval
  ) where

import           Control.Monad
import           Data.Map.Strict

import           Syntax

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Expr
             Env
  deriving (Show)

type Env = Map Name Value

emptyEnv :: Env
emptyEnv = empty

eval :: Env -> Expr -> Value
eval env term =
  case term of
    Var n         -> env ! n
{-
    Lam n a -> VClosure a env
    App a b ->
      let VClosure c env' = eval env a
      in let v = eval env b
         in eval (v : env') c
-}
    Lit (LInt n)  -> VInt n
    Lit (LBool n) -> VBool n
    Op p a b      -> evalPrim p (eval env a) (eval env b)

evalPrim :: BinOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a * b)
evalPrim Sub (VInt a) (VInt b) = VInt (a - b)
evalPrim Eql (VInt a) (VInt b) = VBool (a == b)
