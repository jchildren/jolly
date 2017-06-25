module Pretty where

import           Text.PrettyPrint

import           Eval
import           Syntax

class Pretty p where
  ppr :: Int -> p -> Doc
  pp :: p -> Doc
  pp = ppr 0

{-
viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x
-}
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

instance Pretty Expr where
  ppr p e =
    case e of
      Lit a   -> text (show a)
      Var x   -> text (show x)
      App a b -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b

{-
        Lam x a -> parensIf (p>0) $
                char '\\'
            <>  hsep (fmap pp (viewVars e))
            <+> "->"
            <+> ppr (p+1) (viewBody e)
-}
instance Pretty Value where
  ppr p e =
    case e of
      VInt a       -> text (show a)
      VBool a      -> text (show a)
--      VClosure a b -> ppr (p + 1) a <> hsep (fmap pp b)

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppval :: Value -> String
ppval = render . ppr 0
