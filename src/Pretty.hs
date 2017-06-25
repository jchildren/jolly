module Pretty where

import           Data.Map.Strict
import           Text.PrettyPrint

import           Eval
import           Syntax

class Pretty p where
  ppr :: Int -> p -> Doc
  pp :: p -> Doc
  pp = ppr 0

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

instance Pretty Expr where
  ppr p e =
    case e of
      Lit (LInt a) -> text (show a)
      Lit (LBool a) -> text (show a)
      Var x -> text x
      App a b -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
      Lam x a ->
        parensIf (p > 0) $ char '\\' <> text x <+> text "->" <+> ppr p a
      Op sym a b -> ppr p a <+> showSym sym <+> ppr p b

instance Pretty Value where
  ppr p e =
    case e of
      VInt a -> text (show a)
      VBool a -> text (show a)
      VClosure x a env ->
        char '\\' <> text x <+> text "->" <+> ppr p a <+> showEnv env

showEnv :: Env -> Doc
showEnv e =
  let ppentry (k, a) = text k <+> char '=' <+> pp a
  in braces (hsep (fmap ppentry (assocs e)))

showSym :: BinOp -> Doc
showSym Add = char '+'
showSym Sub = char '-'
showSym Mul = char '*'
showSym Eql = text "=="

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppval :: Value -> String
ppval = render . ppr 0
