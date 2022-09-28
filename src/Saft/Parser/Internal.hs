module Saft.Parser.Internal (Parser, module_) where

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Saft.Ast.Module
import Saft.Ast.Statement
import Saft.Ast.Type as Ty
import Saft.Token as Tk
import qualified Saft.Token as Token
import Text.Megaparsec

type Parser = Parsec Void TokenStream

liftMyToken :: Token.SToken -> Token.WithPos Token.SToken
liftMyToken = Token.WithPos pos pos 0
  where
    pos = initialPos ""

pToken :: SToken -> Parser SToken
pToken c = token test (Set.singleton . Tokens . nes . liftMyToken $ c)
  where
    test (WithPos _ _ _ x) =
      if x == c
        then Just x
        else Nothing
    nes x = x :| []

pIdent :: Parser Text
pIdent = token test Set.empty <?> "identifier"
  where
    test (WithPos _ _ _ (Identifier i)) = Just i
    test _ = Nothing

pBool :: Parser Bool
pBool = token test Set.empty <?> "bool"
  where
    test (WithPos _ _ _ (Tk.Bool b)) = Just b
    test _ = Nothing

pInt :: Parser Text
pInt = token test Set.empty <?> "int"
  where
    test (WithPos _ _ _ (Tk.Int i)) = Just i
    test _ = Nothing

type_ :: Parser Type
type_ =
  choice $
    map
      (\(tok, ty) -> pToken tok $> ty)
      [ (Tk.TVoid, Ty.Void),
        (Tk.TBool, Ty.Bool),
        (Tk.TInt, Ty.Int),
        (Tk.TFloat, Ty.Float)
      ]

module_ :: Parser Module
module_ = Module <$> many statement <* eof

statement :: Parser Statement
statement = fn

fn :: Parser Statement
fn = do
  _ <- pToken Fn
  ident <- pIdent
  _ <- pToken LParen
  arguments <- sepBy ((,) <$> (pIdent <* pToken Colon) <*> type_) (pToken Comma)
  _ <- pToken RParen
  returnType <- option Ty.Void (pToken Arrow *> type_)
  _ <- pToken LBrace
  _ <- pToken RBrace
  return $ Function {identifier = ident, arguments, body = [], returnType}
