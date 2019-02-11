{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.Config.HLint where

import           Control.Applicative             ((<|>))
import           Data.Generics.Labels            ()
import           Distribution.Version
import           GHC.Generics                    (Generic)

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec.Class       as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

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
-- HLintJob
-------------------------------------------------------------------------------

data HLintJob
    = HLintJobLatest    -- ^ run with latest GHC
    | HLintJob Version  -- ^ run with specified GHC version
  deriving (Eq, Show)

instance C.Parsec HLintJob where
    parsec = HLintJobLatest <$ C.string "latest"
        <|> HLintJob <$> C.parsec

instance C.Pretty HLintJob where
    pretty HLintJobLatest = PP.text "latest"
    pretty (HLintJob v)   = C.pretty v

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

hlintConfigGrammar
    :: (C.FieldGrammar g, Applicative (g HLintConfig))
    => g HLintConfig HLintConfig
hlintConfigGrammar = HLintConfig
    <$> C.booleanFieldDef  "hlint"                                             #cfgHLintEnabled False
    <*> C.optionalFieldDef "hlint-job"                                         #cfgHLintJob HLintJobLatest
    <*> C.optionalFieldAla "hlint-yaml"    C.FilePathNT                        #cfgHLintYaml
    <*> C.monoidalFieldAla "hlint-options" (C.alaList' C.NoCommaFSep C.Token') #cfgHLintOptions
    <*> C.optionalFieldDef "hlint-version"                                     #cfgHLintVersion defaultHLintVersion
