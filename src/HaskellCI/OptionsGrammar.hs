module HaskellCI.OptionsGrammar (
    OptionsGrammar (..),
    (C.^^^),
    metaActionHelp,
    ParsecPretty,
    Help, MetaVar, BashCompletionAction
)  where

import HaskellCI.Prelude

import qualified Distribution.Compat.Lens        as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Types.PackageName  as C
import qualified Distribution.Types.VersionRange as C
import qualified Options.Applicative             as O

import HaskellCI.Newtypes

-- | Help text for option.
type Help    = String

-- | Meta variable for option argument.
type MetaVar = String

-- | Bash completion action for option argument.
--   Example: @"file"@ or @"directory"@.
--
-- See <https://github.com/pcapriotti/optparse-applicative#actions-and-completers>
-- and <https://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html>.
type BashCompletionAction = String

class
    ( C.FieldGrammar c p
    , forall s. Applicative (p s)
    , forall a. c a => c (Identity a)
    , c Range, c C.VersionRange
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
    metaCompleterHelp :: MetaVar -> O.Completer -> Help -> p s a -> p s a
    metaCompleterHelp _ _ _ = id

    metahelp :: MetaVar -> Help -> p s a -> p s a
    metahelp _ _ = id

    help :: Help -> p s a -> p s a
    help _ = id

    -- we treat range fields specially in options
    rangeField :: C.FieldName -> C.ALens' s C.VersionRange -> s -> p s C.VersionRange
    rangeField fn l s = C.optionalFieldDefAla fn Range l (C.aview l s)

metaActionHelp :: OptionsGrammar c p => MetaVar -> BashCompletionAction -> Help -> p s a -> p s a
metaActionHelp m a = metaCompleterHelp m (O.bashCompleter a)

instance OptionsGrammar C.Parsec C.ParsecFieldGrammar

class    (C.Parsec a, C.Pretty a) => ParsecPretty a
instance (C.Parsec a, C.Pretty a) => ParsecPretty a
