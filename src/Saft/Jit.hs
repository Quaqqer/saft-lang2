{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Saft.Jit (runJit) where

import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr
import qualified LLVM
import qualified LLVM.AST as LLVMAST
import qualified LLVM.Context as LLVM
import qualified LLVM.OrcJIT as ORC
import qualified LLVM.PassManager as LLVM
import LLVM.Prelude
import qualified LLVM.Target as LLVM

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> IO Int

passes :: LLVM.PassSetSpec
passes = LLVM.defaultCuratedPassSetSpec {LLVM.optLevel = Just 3}

runJit :: LLVMAST.Module -> IO (Maybe Int)
runJit astModule = do
  LLVM.withContext $ \ctx ->
    LLVM.withHostTargetMachineDefault $ \tm ->
      LLVM.withModuleFromAST ctx astModule $ \mod -> do
        -- Run optimization passes
        _ <- LLVM.withPassManager passes $ flip LLVM.runPassManager mod

        ORC.withExecutionSession $ \es -> do
          let dylibName = "saft"
          dylib <- ORC.createJITDylib es dylibName
          ORC.withClonedThreadSafeModule mod $ \tsm -> do
            ol <- ORC.createRTDyldObjectLinkingLayer es
            il <- ORC.createIRCompileLayer es ol tm
            ORC.addModule tsm dylib il

            -- Look up main function
            mainfn <- ORC.lookupSymbol es il dylib "saft_entry"
            case mainfn of
              Right (ORC.JITSymbol fPtr _) -> do
                -- Cast it to a haskell function and run it
                res <- haskFun (castPtrToFunPtr (wordPtrToPtr fPtr))
                return $ Just res
              Left (ORC.JITSymbolError msg) -> do
                -- Something went wrong, this should not happen since the
                -- saft_entry function is a static name
                print msg
                return Nothing
