module Saft.Parser.Statement (pModule) where

import Saft.Ast.Module
import qualified Saft.Ast.Statement as Stmt
import Saft.Ast.Type as Ty
import Saft.Parser.Expression (pExpr)
import Saft.Parser.Internal
import Saft.Token as Tk
import Text.Megaparsec

pModule :: Parser Module
pModule = Module <$> many pOuterStmt <* eof

pOuterStmt :: Parser Stmt.Statement
pOuterStmt = pFn

pInnerStmt :: Parser Stmt.Statement
pInnerStmt = pLet <|> pReturn

pFn :: Parser Stmt.Statement
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

pLet :: Parser Stmt.Statement
pLet = do
  _ <- pToken Let
  identifier <- pIdent
  type_ <- optional (pToken Colon >> pType)
  _ <- pToken Equals
  expr <- pExpr
  _ <- pToken Semicolon
  return Stmt.Let {identifier, type_, expr}

pReturn :: Parser Stmt.Statement
pReturn = do
  _ <- pToken Tk.Return
  expr <- pExpr
  _ <- pToken Semicolon
  return Stmt.Return {expr}
