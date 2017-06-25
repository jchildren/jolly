module Parser
  ( parseExpr
  ) where

import           Control.Monad          (void)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           Syntax

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

boolean :: Parser Bool
boolean = True <$ string "True" <|> False <$ string "False"

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String]
rws = ["let", "if", "then", "else", "where"]

identifier :: Parser Name
identifier = (lexeme .try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` rws
                     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                    else return x

parseExpr :: Parser Expr
parseExpr = between sc eof expr

expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators =
  [[InfixL (Op Mul <$ symbol "*")], [InfixL (Op Add <$ symbol "+")], [InfixL (Op Sub <$ symbol "-")], [InfixL (Op Eql <$ symbol "==")]]

term :: Parser Expr
term =
  parens expr
    <|> Var <$> identifier
   <|> Lam <$> identifier <*> term
   <|> Lit . LInt <$> integer
   <|> Lit . LBool <$> boolean
