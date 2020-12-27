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
    }
  deriving (Show, Generic, Binary)

-------------------------------------------------------------------------------
-- Grammar
-------------------------------------------------------------------------------

docspecConfigGrammar
    :: (OptionsGrammar g, Applicative (g DocspecConfig))
    => g DocspecConfig DocspecConfig
docspecConfigGrammar = DocspecConfig
    <$> rangeField         "docspec"                                              (field @"cfgDocspecEnabled") noVersion
        ^^^ help "Enable Docspec job"
    <*> C.monoidalFieldAla "docspec-options" (C.alaList' C.NoCommaFSep C.Token')  (field @"cfgDocspecOptions")
        ^^^ metahelp "OPTS" "Additional Docspec options"
