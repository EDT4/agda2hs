module Agda2Hs.Compile where

import Control.Monad.Reader ( ReaderT(runReaderT) )

import Agda.Compiler.Backend
import Agda.TypeChecking.Pretty

import Agda2Hs.Compile.ClassInstance ( compileInstance )
import Agda2Hs.Compile.Data ( compileData )
import Agda2Hs.Compile.Function ( compileFun )
import Agda2Hs.Compile.Postulate ( compilePostulate )
import Agda2Hs.Compile.Record ( compileRecord )
import Agda2Hs.Compile.Types
import Agda2Hs.Pragma

initCompileEnv :: CompileEnv
initCompileEnv = CompileEnv { minRecordName = Nothing }

runC :: C a -> TCM a
runC m = runReaderT m initCompileEnv

-- Main compile function
------------------------

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM CompiledDef
compile _ m _ def = withCurrentModule m $ runC $ processPragma (defName def) >>= \ p -> do
  reportSDoc "agda2hs.compile" 5 $ text "Compiling definition: " <+> prettyTCM (defName def)
  case (p , defInstance def , theDef def) of
    (NoPragma           , _      , _         ) -> return []
    (ExistingClassPragma, _      , _         ) -> return [] -- No code generation, but affects how projections are compiled
    (ClassPragma ms     , _      , Record{}  ) -> tag . single <$> compileRecord (ToClass ms) def
    (DerivingPragma ds  , _      , Datatype{}) -> tag <$> compileData ds def
    (DefaultPragma      , _      , Datatype{}) -> tag <$> compileData [] def
    (DefaultPragma      , Just _ , _         ) -> tag . single <$> compileInstance def
    (DefaultPragma      , _      , Axiom{}   ) -> tag <$> compilePostulate def
    (DefaultPragma      , _      , Function{}) -> tag <$> compileFun def
    (DefaultPragma      , _      , Record{}  ) -> tag . single <$> compileRecord ToRecord def
    _                                         -> return []
  where tag code = [(nameBindingSite $ qnameName $ defName def, code)]
        single x = [x]
