module Pretty where

import           Text.PrettyPrint
import           Data.Map.Strict

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
      Lit a   -> text (show a)
      Var x   -> text (show x)
      App a b -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
      Lam x a -> parensIf (p>0) $
                char '\\'
            <>  text x
            <+> text "->"
            <+> ppr p a

instance Pretty Value where
  ppr p e =
    case e of
      VInt a       -> text (show a)
      VBool a      -> text (show a)
      VClosure x a env  -> char '\\'
                        <> text x
                        <+> text "->"
                        <+> ppr p a <> showEnv env

showEnv :: Env -> Doc
showEnv e = braces (hsep (fmap pp (elems e)))

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppval :: Value -> String
ppval = render . ppr 0
