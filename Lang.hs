-- Derived from the llvm in haskell tutorial.

--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Lang where

import Text.PrettyPrint

type Name = String

data Expr
  = Int Integer
  | Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)

pretty :: Expr -> Doc
pretty e = case e of
  Int i -> int (fromIntegral i)
  Float i -> double i
  Var s -> text s
  Call n es -> (text n) <+> hsep (map pretty es)
  Function n args body -> text "def" <+> text n <+> parens(hsep (map text args)) $$ nest 1 (pretty body)
--  Extern id args ->
  BinaryOp "=" e1 e2 -> pretty e1 <+> equals <+> pretty e2
  BinaryOp ":" e1 e2 -> pretty e1 <+> colon $+$ pretty e2
  BinaryOp n e1 e2 -> pretty e1 <+> text n <+> pretty e2
  UnaryOp n e -> text n <+> parens (pretty e)
  If g e1 e2 -> text "if" <+> pretty g $$
                (nest 1 (text "then" <+> pretty e1))
                $+$ (nest 1 (text "else" <+> pretty e2))
  For n initial guard inc body ->
    text "for" <+> text n <+> equals <+> pretty initial <+> comma <+> pretty guard <+> comma <+> pretty inc <+> text "in" $+$ nest 1 (pretty body)
  BinaryDef n args body -> text "def binary" <+> text n <+> parens (hsep (map text args)) <+> pretty body
  UnaryDef n args body -> text "def unary" <+> text n <+> parens (hsep (map text args)) <+> pretty body
  Let n is body -> text "let" <+> text n <+> equals <+> pretty is <+> text "in" $+$ nest 1 (pretty body)
