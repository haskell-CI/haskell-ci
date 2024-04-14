{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module HaskellCI.Config.Docspec (
    DocspecConfig (..),
    docspecConfigGrammar,
) where

import HaskellCI.Prelude

import qualified Distribution.FieldGrammar    as C

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
    , cfgDocspecUrl     = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20240414/cabal-docspec-0.0.0.20240414-x86_64-linux.xz"
    , cfgDocspecHash    = "2d18a3f79619e8ec5f11870f926f6dc2616e02a6c889315b7f82044b95a1adb9"
    }

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

docspecConfigGrammar
    :: (OptionsGrammar c g, Applicative (g DocspecConfig))
    => g DocspecConfig DocspecConfig
docspecConfigGrammar = DocspecConfig
    <$> rangeField            "docspec"                                              (field @"cfgDocspecEnabled") (cfgDocspecEnabled defaultDocspecConfig)
        ^^^ help "Enable Docspec job"
    <*> C.monoidalFieldAla    "docspec-options" (C.alaList' C.NoCommaFSep C.Token')  (field @"cfgDocspecOptions")
        ^^^ metahelp "OPTS" "Additional Docspec options"
    <*> C.optionalFieldDefAla "docspec-url"      C.Token'                            (field @"cfgDocspecUrl") (cfgDocspecUrl defaultDocspecConfig)
        ^^^ metahelp "URL" "URL to download cabal-docspec"
    <*> C.optionalFieldDefAla "docspec-hash"     C.Token'                            (field @"cfgDocspecHash") (cfgDocspecHash defaultDocspecConfig)
        ^^^ metahelp "HASH" "SHA256 of cabal-docspec"
