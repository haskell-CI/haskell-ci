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
    , cfgDocspecUrl     = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20230406/cabal-docspec-0.0.0.20230406-x86_64-linux.xz"
    , cfgDocspecHash    = "68fa9addd5dc453d533a74a763950499d4593b1297c9a05c3ea5bd1acc04c9dd"
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
