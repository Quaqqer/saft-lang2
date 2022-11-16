module Saft.Parser.Outer (pFunction) where

import Data.Maybe (fromMaybe)
import Saft.Ast.Outer as Outer
import Saft.Ast.Type (Type)
import qualified Saft.Ast.Type as Ty
import Saft.Parser.Internal
import Saft.Parser.Statement (pInnerStmt)
import Saft.Token as Tk

typeOrUnknown :: Maybe Type -> Type
typeOrUnknown = fromMaybe Ty.Unknown

pFunction :: Parser (Outer Type)
pFunction = do
  -- Fn
  _ <- pToken Tk.Fn

  -- name
  name <- pIdent

  -- (a: int, b: float, c)
  _ <- pToken Tk.LParen
  args <-
    sepBy
      ( (,)
          <$> pIdent
          <*> (typeOrUnknown <$> optional (pToken Colon *> pType))
      )
      (pToken Comma)
  _ <- pToken Tk.RParen

  returnType <-
    typeOrUnknown
      <$> optional
        ( do
            _ <- pToken Arrow
            pType
        )

  -- { statement; statement; }
  _ <- pToken Tk.LBrace
  body <- many pInnerStmt
  _ <- pToken Tk.RBrace
  return Outer.Function {name, args, returnType, body}

-- pImport :: Parser Outer
-- pExport :: Parser Outer
