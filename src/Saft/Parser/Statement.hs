module Saft.Parser.Statement (pModule) where

import Data.Maybe (fromMaybe)
import Saft.Ast.Module
import qualified Saft.Ast.Statement as Stmt
import Saft.Ast.Type as Ty
import Saft.Parser.Expression (pExpr)
import Saft.Parser.Internal
import Saft.Token as Tk
import Text.Megaparsec

pModule :: Parser (Module Type)
pModule = Module <$> many pOuterStmt <* eof

pOuterStmt :: Parser (Stmt.Statement Type)
pOuterStmt = pFn

pInnerStmt :: Parser (Stmt.Statement Type)
pInnerStmt = pLet <|> pReturn

pFn :: Parser (Stmt.Statement Type)
pFn = do
  _ <- pToken Fn
  ident <- pIdent

  -- Arguments
  _ <- pToken LParen
  arguments <- sepBy ((,) <$> (pIdent <* pToken Colon) <*> pType) (pToken Comma)
  _ <- pToken RParen

  -- Return type
  returnType <- option Ty.Void (pToken Arrow *> pType)

  -- Body
  _ <- pToken LBrace
  body <- many pInnerStmt
  _ <- pToken RBrace

  return Stmt.Function {identifier = ident, arguments, body, returnType}

pLet :: Parser (Stmt.Statement Type)
pLet = do
  _ <- pToken Let
  identifier <- pIdent
  maybe_type <- optional (pToken Colon >> pType)
  let type_ = fromMaybe Unknown maybe_type
  _ <- pToken Equals
  expr <- pExpr
  _ <- pToken Semicolon
  return Stmt.Let {identifier, type_, expr}

pReturn :: Parser (Stmt.Statement Type)
pReturn = do
  _ <- pToken Tk.Return
  expr <- pExpr
  _ <- pToken Semicolon
  return Stmt.Return {expr}
