{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.HLint where

import HaskellCI.Prelude

import Distribution.Version (withinVersion)

import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

import HaskellCI.OptionsGrammar

data HLintConfig = HLintConfig
    { cfgHLintEnabled  :: !Bool
    , cfgHLintJob      :: !HLintJob
    , cfgHLintYaml     :: !(Maybe FilePath)
    , cfgHLintOptions  :: [String]
    , cfgHLintVersion  :: !VersionRange
    , cfgHLintDownload :: !Bool
    }
  deriving (Show, Generic)

defaultHLintVersion :: VersionRange
defaultHLintVersion = withinVersion (mkVersion [3,1])

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
    :: (OptionsGrammar g, Applicative (g HLintConfig))
    => g HLintConfig HLintConfig
hlintConfigGrammar = HLintConfig
    <$> C.booleanFieldDef  "hlint"                                             (field @"cfgHLintEnabled") False
        ^^^ help "Enable HLint job"
    <*> C.optionalFieldDef "hlint-job"                                         (field @"cfgHLintJob") HLintJobLatest
        ^^^ metahelp "JOB" "Specify HLint job"
    <*> C.optionalFieldAla "hlint-yaml"    C.FilePathNT                        (field @"cfgHLintYaml")
        ^^^ metahelp "PATH" "Use specific .hlint.yaml"
    <*> C.monoidalFieldAla "hlint-options" (C.alaList' C.NoCommaFSep C.Token') (field @"cfgHLintOptions")
        ^^^ metahelp "OPTS" "Additional HLint options"
    <*> C.optionalFieldDef "hlint-version"                                     (field @"cfgHLintVersion") defaultHLintVersion
        ^^^ metahelp "RANGE" "HLint version"
    <*> C.booleanFieldDef "hlint-download-binary"                              (field @"cfgHLintDownload") True
        ^^^ help "Download HLint binary release"
