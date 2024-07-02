{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Docspec (
    DocspecConfig (..),
    docspecConfigGrammar,
    initialDocspecConfig,
) where

import HaskellCI.Prelude

import qualified Distribution.FieldGrammar as C

import HaskellCI.GrammarDefault
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

initialDocspecConfig :: DocspecConfig
initialDocspecConfig = DocspecConfig
    { cfgDocspecEnabled = noVersion
    , cfgDocspecOptions = []
    , cfgDocspecUrl     = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20240414/cabal-docspec-0.0.0.20240414-x86_64-linux.xz"
    , cfgDocspecHash    = "2d18a3f79619e8ec5f11870f926f6dc2616e02a6c889315b7f82044b95a1adb9"
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

docspecConfigGrammar :: OptionsGrammar c g => DocspecConfig -> g DocspecConfig DocspecConfig
docspecConfigGrammar def = DocspecConfig
    <$> rangeField            "docspec"                                            (field @"cfgDocspecEnabled") def
        ^^^ help "Enable Docspec job"
    <*> monoidalFieldAla    "docspec-options" (C.alaList' C.NoCommaFSep C.Token')  (field @"cfgDocspecOptions")
        ^^^ metahelp "OPTS" "Additional Docspec options"
    <*> optionalFieldDefAla "docspec-url"      C.Token'                            (field @"cfgDocspecUrl") def
        ^^^ metahelp "URL" "URL to download cabal-docspec"
    <*> optionalFieldDefAla "docspec-hash"     C.Token'                            (field @"cfgDocspecHash") def
        ^^^ metahelp "HASH" "SHA256 of cabal-docspec"
