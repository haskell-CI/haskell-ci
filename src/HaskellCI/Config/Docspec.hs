{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Docspec where

import HaskellCI.Prelude

import qualified Distribution.FieldGrammar    as C
import qualified Distribution.Parsec.Newtypes as C

import HaskellCI.OptionsGrammar

data DocspecConfig = DocspecConfig
    { cfgDocspecEnabled :: !VersionRange
    , cfgDocspecOptions :: [String]
    , cfgDocspecUrl     :: String
    , cfgDocspecHash    :: String
    }
  deriving (Show, Generic, Binary)

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

defaultDocspecConfig :: DocspecConfig
defaultDocspecConfig = DocspecConfig
    { cfgDocspecEnabled = noVersion
    , cfgDocspecOptions = []
    , cfgDocspecUrl     = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20201230.1/cabal-docspec-0.0.0.20201230.1.xz"
    , cfgDocspecHash    = "18caf4f361fadd978782f08e78f42d21d4f177567419055ffccae19b8214852d"
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

docspecConfigGrammar
    :: (OptionsGrammar g, Applicative (g DocspecConfig))
    => g DocspecConfig DocspecConfig
docspecConfigGrammar = DocspecConfig
    <$> rangeField            "docspec"                                              (field @"cfgDocspecEnabled") noVersion
        ^^^ help "Enable Docspec job"
    <*> C.monoidalFieldAla    "docspec-options" (C.alaList' C.NoCommaFSep C.Token')  (field @"cfgDocspecOptions")
        ^^^ metahelp "OPTS" "Additional Docspec options"
    <*> C.optionalFieldDefAla "docspec-url"      C.Token'                            (field @"cfgDocspecUrl") (cfgDocspecUrl defaultDocspecConfig)
        ^^^ metahelp "URL" "URL to download cabal-docspec"
    <*> C.optionalFieldDefAla "docspec-hash"     C.Token'                            (field @"cfgDocspecHash") (cfgDocspecHash defaultDocspecConfig)
        ^^^ metahelp "HASH" "SHA256 of cabal-docspec"
