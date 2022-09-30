module Saft.Parser.Internal (Parser, liftMyToken, pToken, pIdent, pType) where

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Saft.Ast.Type as Ty
import Saft.Token as Tk
import Text.Megaparsec

type Parser = Parsec Void TokenStream

liftMyToken :: Tk.SToken -> Tk.WithPos Tk.SToken
liftMyToken = Tk.WithPos pos pos 0
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

pType :: Parser Type
pType =
  choice $
    map
      (\(tok, ty) -> pToken tok $> ty)
      [ (Tk.TVoid, Ty.Void),
        (Tk.TBool, Ty.Bool),
        (Tk.TInt, Ty.Int),
        (Tk.TFloat, Ty.Float)
      ]
