{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Saft.Compiler (generateIR, printIR, compileIR) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified LLVM
import qualified LLVM.AST as LLVMAST
import qualified LLVM.AST.Type as LLVMAST
import qualified LLVM.Context as LLVM
import LLVM.IRBuilder.Constant as LLVMIR
import LLVM.IRBuilder.Instruction as LLVMIR
import LLVM.IRBuilder.Module as LLVMIR
import LLVM.IRBuilder.Monad as LLVMIR
import qualified LLVM.Target as LLVM
import qualified Saft.Ast.Module as S
import qualified Saft.Ast.Statement as S
import qualified Saft.Ast.Type as S

llvmType :: S.Type -> LLVMAST.Type
llvmType t = fromJust $ Map.lookup t m
  where
    m = Map.fromList typeEdges
    typeEdges =
      [ (S.Void, LLVMAST.void),
        (S.Bool, LLVMAST.i1),
        (S.Int, LLVMAST.i32),
        (S.Float, LLVMAST.double)
      ]

generateIR :: String -> Text -> S.Module -> LLVMAST.Module
generateIR name mainIs (S.Module {body}) =
  buildModule "saft.ll" $ do
    fns <-
      Map.fromList
        <$> mapM
          ( \stmt@(S.Function {identifier}) -> do
              fn <- generateOuter stmt
              return (identifier, fn)
          )
          body

    generateMain (fromJust (Map.lookup mainIs fns))

generateOuter :: S.Statement -> LLVMIR.ModuleBuilder LLVMAST.Operand
generateOuter
  ( S.Function
      { identifier,
        arguments,
        body,
        returnType
      }
    ) =
    do
      function
        (LLVMAST.mkName ("s_" ++ Text.unpack identifier))
        (map (\(id', ty) -> (llvmType ty, fromString (Text.unpack id'))) arguments)
        (llvmType returnType)
        $ \_ -> do
          _entry <- block `named` "entry"
          retVoid

generateMain :: LLVMAST.Operand -> LLVMIR.ModuleBuilder ()
generateMain mainIs = do
  _ <- function
    "saft_entry"
    []
    (LLVMAST.IntegerType 32)
    $ \[] -> do
      _entry <- block `named` "entry"
      _ <- call mainIs []
      ret $ int32 123

  return ()

printIR :: LLVMAST.Module -> IO ()
printIR mod' = LLVM.withContext $ \ctx -> do
  llvm <- LLVM.withModuleFromAST ctx mod' LLVM.moduleLLVMAssembly
  BS.putStrLn llvm

compileIR :: String -> LLVMAST.Module -> IO ()
compileIR name moduleAST = LLVM.withContext $ \ctx -> do
  LLVM.withModuleFromAST ctx moduleAST $ \module_ -> do
    LLVM.withHostTargetMachineDefault $ \target -> do
      LLVM.writeObjectToFile target (LLVM.File name) module_
