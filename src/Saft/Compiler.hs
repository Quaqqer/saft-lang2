{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Saft.Compiler (generateIR, printIR, compileIR) where

import qualified Data.ByteString.Char8 as BS
import Data.Functor.Identity (Identity)
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
import qualified Saft.Ast.Expression as SE
import qualified Saft.Ast.Module as SM
import qualified Saft.Ast.Statement as SS
import qualified Saft.Ast.Type as ST

llvmType :: ST.Type -> LLVMAST.Type
llvmType t = fromJust $ Map.lookup t m
  where
    m = Map.fromList typeEdges
    typeEdges =
      [ (ST.Void, LLVMAST.void),
        (ST.Bool, LLVMAST.i1),
        (ST.Int, LLVMAST.i32),
        (ST.Float, LLVMAST.double)
      ]

generateIR :: String -> Text -> SM.Module -> LLVMAST.Module
generateIR name mainIs (SM.Module {body}) =
  buildModule "saft.ll" $ do
    fns <-
      Map.fromList
        <$> mapM
          ( \stmt@(SS.Function {identifier}) -> do
              fn <- generateOuter stmt
              return (identifier, fn)
          )
          body

    generateMain (fromJust (Map.lookup mainIs fns))

generateOuter :: SS.Statement -> LLVMIR.ModuleBuilder LLVMAST.Operand
generateOuter
  ( SS.Function
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
        $ \_args -> do
          _entry <- block `named` "entry"
          mapM_ generateInner body
generateOuter stmt = error $ "Unexpected outer statment " ++ show stmt

generateInner :: SS.Statement -> LLVMIR.IRBuilderT LLVMIR.ModuleBuilder ()
generateInner (SS.Return {expr = SE.Void}) = retVoid
generateInner (SS.Return {expr}) = ret $ generateExpr expr
generateInner stmt = error $ "Unexpected inner statement " ++ show stmt

generateExpr :: SE.Expression -> LLVMAST.Operand
generateExpr (SE.Bool b) = bit (if b then 1 else 0)
generateExpr (SE.Int i) = int32 (read $ Text.unpack i)
generateExpr (SE.Float f) = double (read $ Text.unpack f)
generateExpr SE.Void = error "Void cannot be converted to a LLVM value."

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
