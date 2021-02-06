{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module HaskellCI.OptionsGrammar (
    OptionsGrammar (..),
    (C.^^^),
    ParsecPretty,
)  where

import HaskellCI.Prelude

import qualified Distribution.Compat.Lens        as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.PackageName  as C
import qualified Distribution.Types.VersionRange as C

import HaskellCI.Newtypes

class
    ( C.FieldGrammar c p
    , c Range, c (Identity C.VersionRange)
    , c (C.List C.NoCommaFSep C.Token' String)
    , c (C.List C.FSep C.Token' String)
    , c (AlaSet C.NoCommaFSep C.Token' String)
    , c (AlaSet C.NoCommaFSep (Identity Version) Version)
    , c (C.List C.CommaVCat NoCommas String)
    , c (C.List C.NoCommaFSep (Identity C.PackageName) C.PackageName)
    , c (C.List C.FSep (Identity C.PackageName) C.PackageName)
    )
    => OptionsGrammar c p | p -> c
  where
    metahelp :: String -> String -> p s a -> p s a
    metahelp _ _ = id

    help :: String -> p s a -> p s a
    help _ = id

    -- we treat range fields specially in options
    rangeField :: C.FieldName -> C.ALens' s C.VersionRange -> C.VersionRange -> p s C.VersionRange
    rangeField fn = C.optionalFieldDefAla fn Range

instance OptionsGrammar C.Parsec C.ParsecFieldGrammar

class    (C.Parsec a, C.Pretty a) => ParsecPretty a
instance (C.Parsec a, C.Pretty a) => ParsecPretty a
