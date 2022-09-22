module Saft.Parser.Internal (Parser) where

import Data.Void (Void)
import qualified Saft.Token as Token
import Text.Megaparsec
-- import Saft.Ast.Statement
-- import Saft.Token
-- import qualified Saft.Ast.Statement as Ast.Stmt
-- import qualified Saft.Ast.Expression as Ast.Expr

type Parser = Parsec Void Token.SToken

-- statement :: Parser Statement
-- statement = fn

-- fn :: Parser Statement
-- fn = do
--   _ <- char Fn
--   ident <- anySingle
--   fail "I expected an identifier"

-- statement :: Parser Ast.Stmt.Statement
-- statement = 
--
-- function :: Parser Ast.Stmt.Statement
-- function = char Let
