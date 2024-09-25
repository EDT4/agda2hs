{-# LANGUAGE OverloadedStrings #-}
-- Read additional configuration options from a file.
module Agda2Hs.Config (checkConfig) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import GHC.Generics

import Data.Functor ( (<&>) )
import Data.Foldable ( fold )
import Data.Maybe ( fromMaybe )
import Data.Aeson ( FromJSON(parseJSON), withObject, (.:), (.:?), (.!=) )
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml

import Agda2Hs.Compile.Types
import Agda2Hs.Compile.Name ( toNameImport )
import Agda2Hs.HsUtils ( hsModuleName )
import Agda.TypeChecking.Monad.Base ( TCM, genericDocError )
import Agda.Syntax.Common.Pretty

-- | Config file data.
data Config = Config
  { cfgPrelude  :: Maybe PreludeOptions
  , cfgRewrites :: Maybe Rewrites
  , cfgModRewrites :: Maybe ModRewrites
  }

instance FromJSON ModRewrite where
  parseJSON = withObject "module rewrite rule" $ \v ->
    ModRewrite <$> v .:  "from"
               <*> v .:  "to"

instance FromJSON Rewrite where
  parseJSON = withObject "rewrite rule" $ \v ->
    Rewrite <$> v .:  "from"
            <*> v .:  "to"
            <*> v .:? "importing"

instance FromJSON PreludeOptions where
  parseJSON = withObject "prelude options" $ \v ->
    PreludeOpts <$> v .:  "implicit"
                <*> v .:? "using"
                <*> v .:? "hiding" .!= []

instance FromJSON Config where
  parseJSON (Aeson.Object v) =
    Config <$> v .:? "prelude"
           <*> v .:? "rewrites"
           <*> v .:? "module-rewrites"
  parseJSON Aeson.Null = pure $ Config Nothing Nothing Nothing
  parseJSON invalid =
    Aeson.prependFailure "parsing agda2hs config failed, "
      (Aeson.typeMismatch "Object" invalid)

readConfigFile :: FilePath -> TCM Config
readConfigFile src = do
  liftIO (Yaml.decodeFileEither src) >>= \case
    Left err  -> genericDocError $ vcat
      [ text "An error occured while trying to parse config file " <> text src <>":"
      , multiLineText (Yaml.prettyPrintParseException err)
      ]
    Right cfg -> return cfg

applyConfig :: Options -> Config -> Options
applyConfig opts cfg =
  opts { optPrelude  = fromMaybe (optPrelude opts) (cfgPrelude cfg)
       , optRewrites = foldl addRewrite (optRewrites opts) (fold $ cfgRewrites cfg)
       , optModRewrites = foldl addModRewrite (optModRewrites opts) (fold $ cfgModRewrites cfg)
       }
  where addRewrite :: SpecialRules -> Rewrite -> SpecialRules
        addRewrite rules (Rewrite from to importing) = Map.insert from (toNameImport to importing) rules

        addModRewrite :: ModSpecialRules -> ModRewrite -> ModSpecialRules
        addModRewrite rules (ModRewrite from to) = Map.insert from (hsModuleName to) rules


checkConfig :: Options -> TCM Options
checkConfig opts
  | Just src <- optConfigFile opts
  = readConfigFile src <&> applyConfig opts
checkConfig opts = return opts

