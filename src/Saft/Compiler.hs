{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Saft.Compiler (generateIR, printIR, compileIR) where

import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
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

generateIR :: String -> String -> SM.Module ST.Type -> LLVMAST.Module
generateIR moduleName mainIs (SM.Module {body}) =
  buildModule "saft.ll" $ mdo
    globals <-
      Map.fromList
        <$> mapM
          ( \stmt@(SS.Function {identifier}) -> do
              fn <- generateOuter globals stmt
              return (identifier, fn)
          )
          body

    generateMain (fromJust (Map.lookup mainIs globals))

generateOuter ::
  Map String LLVMAST.Operand ->
  SS.Statement ST.Type ->
  LLVMIR.ModuleBuilder LLVMAST.Operand
generateOuter
  globals
  ( SS.Function
      { identifier,
        arguments,
        body,
        returnType
      }
    ) =
    do
      function
        (LLVMAST.mkName ("s_" ++ identifier))
        (map (\(id', ty) -> (llvmType ty, fromString id')) arguments)
        (llvmType returnType)
        $ \_args -> do
          _entry <- block `named` "entry"
          mapM_ (generateInner globals) body
generateOuter _ stmt = error $ "Unexpected outer statment " ++ show stmt

generateInner ::
  Map String LLVMAST.Operand ->
  SS.Statement ST.Type->
  LLVMIR.IRBuilderT LLVMIR.ModuleBuilder ()
generateInner _ (SS.Return {expr = SE.Void}) = retVoid
generateInner globals (SS.Return {expr}) = do
  op <- generateExpr globals expr
  ret op
generateInner globals (SS.Let {identifier, type_, expr}) = do
  ptr <- alloca (llvmType type_) Nothing 0
  val <- generateExpr globals expr
  store ptr 0 val
generateInner _ stmt = error $ "Unexpected inner statement " ++ show stmt

generateExpr ::
  Map String LLVMAST.Operand ->
  SE.Expression ->
  IRBuilderT ModuleBuilder LLVMAST.Operand
generateExpr _ (SE.Bool b) = return $ bit (if b then 1 else 0)
generateExpr _ (SE.Int i) = return $ int32 (read i)
generateExpr _ (SE.Float f) = return $ double (read f)
generateExpr globals (SE.Var i) = return $ fromJust $ Map.lookup i globals
generateExpr globals (SE.Call {identifier, arguments}) = do
  args <- mapM (fmap (,[]) . generateExpr globals) arguments
  call (fromJust $ Map.lookup identifier globals) args
generateExpr _ SE.Void = error "Void cannot be converted to a LLVM value."

-- TODO: The main function should be volatile to avoid LLVM optimizing away
-- parameters for when I decide to pass argc and argv
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
