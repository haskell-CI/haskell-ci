{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.OptionsGrammar (
    OptionsGrammar,
    runOptionsGrammar,
    ) where

import           Prelude                     ()
import           Prelude.Compat

import           Control.Applicative         (liftA2, many, optional)
import           Data.Maybe                  (fromMaybe)
import           Distribution.Simple.Utils   (fromUTF8BS)

import qualified Data.Map.Strict             as M
import qualified Distribution.Compat.Lens    as C
import qualified Distribution.Compat.Newtype as C
import qualified Distribution.FieldGrammar   as C
import qualified Distribution.Parsec.Class   as C
import qualified Distribution.Parsec.Field   as C
import qualified Options.Applicative         as O

-- import qualified Distribution.Pretty         as C

newtype OptionsGrammar s a = OG { runOptionsGrammar :: O.Parser (s -> s) }
  deriving Functor

instance Applicative (OptionsGrammar s) where
    pure _ = OG (pure id)
    OG f <*> OG x = OG (liftA2 (.) f x)

instance C.FieldGrammar OptionsGrammar where
    blurFieldGrammar l (OG p) = OG (fmap (l C.#%~) p)

    uniqueFieldAla fn c l =
        OG $ setOG l $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn []

    -- TODO: Manually prepend default to help
    booleanFieldDef fn l _def =
        OG $ maybe id id <$> optional og
      where
        og = setOG l $ O.option readMParsec $ optionMods fn []

    optionalFieldAla fn c l =
        OG $ setOptionalOG l $ optional $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn []

    -- TODO: Manually prepend default to help
    optionalFieldDefAla fn c l _def =
        OG $ maybe id id <$> optional og
      where
        og = setOG l $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn []

    monoidalFieldAla fn c l =
        OG $ monoidOG l $ many $ O.option (C.unpack' c <$> readMParsec) $ optionMods fn []

    prefixedFields _ _   = pure []
    knownField _         = pure ()
    deprecatedSince _  _ = id
    availableSince _ _   = id
    hiddenField          = id

optionMods :: C.FieldName -> [O.Mod O.OptionFields a] -> O.Mod O.OptionFields a
optionMods fn mods = O.long (fromUTF8BS fn)
    <> fromMaybe mempty (M.lookup fn extraMods)
    <> mconcat mods

readMParsec :: C.Parsec a => O.ReadM a
readMParsec = O.eitherReader C.eitherParsec

setOG :: C.ALens' s a -> O.Parser a -> O.Parser (s -> s)
setOG l = fmap (l C.#~)

setOptionalOG :: C.ALens' s (Maybe a) -> O.Parser (Maybe a) -> O.Parser (s -> s)
setOptionalOG l = fmap $ maybe id $ \x -> l C.#~ Just x

monoidOG :: Monoid a => C.ALens' s a -> O.Parser [a] -> O.Parser (s -> s)
monoidOG l = fmap $ \xs -> l C.#%~ \x -> mconcat (x : xs)

-------------------------------------------------------------------------------
-- Help texts
-------------------------------------------------------------------------------

extraMods :: M.Map C.FieldName (O.Mod O.OptionFields a)
extraMods = M.fromList
    [ mk "cabal-install-version"     "VERSION"  "cabal-install version for all jobs"
    , mk "jobs"                      "JOBS"     "jobs (N:M - cabal:ghc)"
    , mk "local-ghc-options"         "OPTS"     "--ghc-options for local packages"
    , mk "cache"                     "BOOL"     "Caching"
    , mk "cabal-check"               "BOOL"     "Cabal 'noisy' output"
    , mk "cabal-noise"               "BOOL"     "Run cabal check"
    , mk "no-test-no-benchmarks"     "BOOL"     "Build with --disable-tests --disable-benchmarks"  
    , mk "install-dependencies-step" "BOOL"     "Install dependencies in a separate step"
    , mk "branches"                  "BRANCH"   "Enable builds only for specirfic branches"
    , mk "irc-channels"              "IRC"      "enable IRC notifications to given channel (e.g. 'irc.freenode.org#haskell-lens')"
    , mk "project-name"              "NAME"     "project name (used for IRC notifications), defaults to package name or name of first package listed in cabal.project file"
    , mk "folds"                     "FOLDS"    "build sections to fold"
    , mk "ghc-head"                  "BOOL"     "Build also with ghc-head"
    , mk "env"                       "ENV"      "Environment variables per job (e.g. `8.0.2:HADDOCK=false`)"
    , mk "allow-failures"            "JOB"      "Allow failures of particular GHC version"
    , mk "last-in-series"            "BOOL"     "[Discouraged] Assume there are only GHCs last in major series: 8.0.* will match only 8.2.2"
    , mk "osx"                       "JOB"      "Jobs to build with OSX too"
    , mk "apt"                       "PKG"      "Additional apt packages to install"

    , mk "doctest"                   "BOOL"     "Run doctest"
    , mk "doctest-options"           "ARG"      "Additional doctest options"
    , mk "doctest-version"           "RANGE"    "doctest executable version range"

    , mk "hlint"                     "BOOL"     "Run HLint"
    , mk "hlint-job"                 "VERSION"  "Job where to run HLint"
    , mk "hlint-yaml"                "PATH"     "Relative path to .hlint.yaml"
    , mk "hlint-options"             "ARG"      "Additional hlint options"
    , mk "hlint-version"             "RANGE"    "HLint executable version range"
    
    ]
  where
    mk fn m h = (fn, O.help h <> O.metavar m)
