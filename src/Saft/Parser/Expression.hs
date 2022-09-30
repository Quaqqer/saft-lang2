module Saft.Parser.Expression (pExpr) where

import qualified Data.Set as Set
import qualified Saft.Ast.Expression as Expr
import Saft.Parser.Internal
import Saft.Token as Tk
import Text.Megaparsec

pExpr :: Parser Expr.Expression
pExpr =
  pVoid
    <|> pBool
    <|> pInt
    <|> pFloat

pVoid :: Parser Expr.Expression
pVoid = Expr.Void <$ pToken TVoid

pBool :: Parser Expr.Expression
pBool = Expr.Bool <$> token test Set.empty <?> "bool"
  where
    test (WithPos _ _ _ (Tk.Bool b)) = Just b
    test _ = Nothing

pInt :: Parser Expr.Expression
pInt = Expr.Int <$> token test Set.empty <?> "int"
  where
    test (WithPos _ _ _ (Int i)) = Just i
    test _ = Nothing

pFloat :: Parser Expr.Expression
pFloat = Expr.Float <$> token test Set.empty <?> "float"
  where
    test (WithPos _ _ _ (Float f)) = Just f
    test _ = Nothing
