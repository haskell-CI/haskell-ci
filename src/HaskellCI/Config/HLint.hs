{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.HLint where

import           Data.Generics.Labels         ()
import           Distribution.Version
import           GHC.Generics                 (Generic)

import qualified Distribution.FieldGrammar    as C
import qualified Distribution.Parsec.Newtypes as C

data HLintJob
    = HLintJobLatest    -- ^ run with latest GHC
    | HLintJob Version  -- ^ run with specified GHC version
  deriving Show

data HLintConfig = HLintConfig
    { cfgHLintEnabled :: !Bool
    , cfgHLintJob     :: !HLintJob
    , cfgHLintYaml    :: !(Maybe FilePath)
    , cfgHLintOptions :: [String]
    , cfgHLintVersion :: !VersionRange
    }
  deriving (Show, Generic)

defaultHLintVersion :: VersionRange
defaultHLintVersion = withinVersion (mkVersion [2,1])

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

hlintConfigGrammar
    :: (C.FieldGrammar g, Applicative (g HLintConfig))
    => g HLintConfig HLintConfig
hlintConfigGrammar = HLintConfig
    <$> C.booleanFieldDef  "hlint"                                             #cfgHLintEnabled False
    <*> pure HLintJobLatest -- TODO
    <*> C.optionalFieldAla "hlint-yaml"    C.FilePathNT                        #cfgHLintYaml
    <*> C.monoidalFieldAla "hlint-options" (C.alaList' C.NoCommaFSep C.Token') #cfgHLintOptions
    <*> C.optionalFieldDef "hlint-version"                                     #cfgHLintVersion defaultHLintVersion
