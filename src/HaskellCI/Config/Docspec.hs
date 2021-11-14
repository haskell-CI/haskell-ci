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
    , cfgDocspecUrl     = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20211114/cabal-docspec-0.0.0.20211114.xz"
    , cfgDocspecHash    = "e224700d9e8c9ec7ec6bc3f542ba433cd9925a5d356676c62a9bd1f2c8be8f8a"
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
