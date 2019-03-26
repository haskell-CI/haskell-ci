{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
-- | @travis.yaml@ structure.
module HaskellCI.Travis.Yaml where

import Control.Monad      (unless)
import Data.Foldable      (for_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String        (fromString)

import qualified Data.Aeson         as Aeson
import qualified Data.List.NonEmpty as NE

import HaskellCI.List
import HaskellCI.Sh
import HaskellCI.YamlSyntax

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Travis = Travis
    { travisLanguage      :: String
    , travisDist          :: String
    , travisGit           :: TravisGit
    , travisCache         :: TravisCache
    , travisBranches      :: TravisBranches
    , travisNotifications :: TravisNotifications
    , travisServices      :: [String]
    , travisAddons        :: TravisAddons
    , travisMatrix        :: TravisMatrix
    , travisBeforeCache   :: [Sh]
    , travisBeforeInstall :: [Sh]
    , travisInstall       :: [Sh]
    , travisScript        :: [Sh]
    }
  deriving Show

newtype TravisGit = TravisGit
    { tgSubmodules :: Bool
    }
  deriving Show

newtype TravisCache = TravisCache
    { tcDirectories :: [FilePath]
    }
  deriving Show

newtype TravisBranches = TravisBranches
    { tbOnly :: [String]
    }
  deriving Show

newtype TravisNotifications = TravisNotifications
    { tnIRC :: Maybe TravisIRC
    }
  deriving Show

data TravisIRC = TravisIRC
    { tiChannels :: [String]
    , tiSkipJoin :: Bool
    , tiTemplate :: [String]
    }
  deriving Show

data TravisMatrix = TravisMatrix
    { tmInclude       :: [TravisJob]
    , tmAllowFailures :: [TravisAllowFailure]
    }
  deriving Show

data TravisJob = TravisJob
    { tjCompiler :: String
    , tjEnv      :: Maybe String
    , tjAddons   :: TravisAddons
    , tjOS       :: String
    }
  deriving Show

data TravisAddons = TravisAddons
    { taApt      :: TravisApt
    , taPostgres :: Maybe String
    }
  deriving Show

data TravisApt = TravisApt
    { taPackages :: [String]
    , taSources  :: [String]
    }
  deriving Show

newtype TravisAllowFailure = TravisAllowFailure
    { tafCompiler :: String
    }
  deriving Show

-------------------------------------------------------------------------------
-- Serialisation helpers (move to Travis.Yaml?)
-------------------------------------------------------------------------------

(~>) :: String -> Yaml [String] -> ([String], String, Yaml [String])
k ~> v = ([],k,v)

(^^^) :: ([String], String, Yaml [String]) -> String -> ([String], String, Yaml [String])
(a,b,c) ^^^ d = (d : a, b, c)

shListToYaml :: [Sh] -> Yaml [String]
shListToYaml shs = YList [] $ concat
    [ YString cs x : map fromString xs
    | (cs, x :| xs) <- gr shs
    ]
  where
    gr :: [Sh] -> [([String], NonEmpty String)]
    gr [] = []
    gr (Sh x : rest) = case gr rest of
        ([], xs) : xss -> ([], NE.cons x xs) : xss
        xss            -> ([], pure x) : xss

    gr (Comment c : rest) = case gr rest of
        (cs, xs) : xss -> (c : cs, xs) : xss
        []             -> [] -- end of comments are lost

ykeyValuesFilt :: ann -> [(ann, String, Yaml ann)] -> Yaml ann
ykeyValuesFilt ann xs = YKeyValues ann
    [ x
    | x@(_,_,y)  <- xs
    , not (isEmpty y)
    ]

ylistFilt :: ann -> [Yaml ann] -> Yaml ann
ylistFilt ann xs = YList ann
    [ x
    | x <- xs
    , not (isEmpty x)
    ]

isEmpty :: Yaml ann -> Bool
isEmpty (YList _ [])      = True
isEmpty (YKeyValues _ []) = True
isEmpty _                 = False

-------------------------------------------------------------------------------
-- ToYaml
-------------------------------------------------------------------------------

instance ToYaml Travis where
    toYaml Travis {..} = ykeyValuesFilt []
        [ "language"       ~> fromString travisLanguage
        , "dist"           ~> fromString travisDist
        , "git"            ~> toYaml travisGit
        , "branches"       ~> toYaml travisBranches
        , "notifications"  ~> toYaml travisNotifications
        , "services"       ~> YList [] (map fromString travisServices)
        , "addons"         ~> toYaml travisAddons
        , "cache"          ~> toYaml travisCache
        , "before_cache"   ~> shListToYaml travisBeforeCache
        , "matrix"         ~> toYaml travisMatrix
        , "before_install" ~> shListToYaml travisBeforeInstall
        , "install"        ~> shListToYaml travisInstall
        , "script"         ~> shListToYaml travisScript
        ]

instance ToYaml TravisGit where
    toYaml TravisGit {..} = ykeyValuesFilt []
        [ "submodules" ~> toYaml tgSubmodules
          ^^^ "whether to recursively clone submodules"
        ]

instance ToYaml TravisBranches where
    toYaml TravisBranches {..} = ykeyValuesFilt []
        [ "only" ~> ylistFilt [] (map fromString tbOnly)
        ]

instance ToYaml TravisNotifications where
    toYaml TravisNotifications {..} = ykeyValuesFilt [] $ buildList $
        for_ tnIRC $ \y -> item $ "irc" ~> toYaml y

instance ToYaml TravisIRC where
    toYaml TravisIRC {..} = ykeyValuesFilt []
        [ "channels"  ~> YList [] (map fromString tiChannels)
        , "skip_join" ~> toYaml tiSkipJoin
        , "template"  ~> YList [] (map fromString tiTemplate)
        ]

instance ToYaml TravisCache where
    toYaml TravisCache {..} = ykeyValuesFilt []
        [ "directories" ~> ylistFilt []
            [ fromString d
            | d <- tcDirectories
            ]
        ]

instance ToYaml TravisMatrix where
    toYaml TravisMatrix {..} = ykeyValuesFilt []
        [ "include"        ~> ylistFilt [] (map toYaml tmInclude)
        , "allow_failures" ~> ylistFilt [] (map toYaml tmAllowFailures)
        ]

instance ToYaml TravisJob where
    toYaml TravisJob {..} = ykeyValuesFilt [] $ buildList $ do
        item $ "compiler" ~> fromString tjCompiler
        item $ "addons" ~> toYaml (Aeson.toJSON tjAddons)
        for_ tjEnv $ \e ->
            item $ "env" ~> fromString e
        unless (tjOS == "linux") $
            item $ "os" ~> fromString tjOS

instance ToYaml TravisAllowFailure where
    toYaml TravisAllowFailure {..} = ykeyValuesFilt []
        [ "compiler" ~> fromString tafCompiler
        ]

instance ToYaml TravisAddons where
    toYaml TravisAddons {..} = ykeyValuesFilt [] $ buildList $ do
        -- no apt on purpose
        for_ taPostgres $ \p -> 
            item $ "postgresql" ~> fromString p

-------------------------------------------------------------------------------
-- ToJSON
-------------------------------------------------------------------------------

instance Aeson.ToJSON TravisAddons where
    -- no postgresql on purpose
    toJSON TravisAddons {..} = Aeson.object
        [ "apt" Aeson..= taApt
        ]

instance Aeson.ToJSON TravisApt where
    toJSON TravisApt {..} = Aeson.object
        [ "packages" Aeson..= taPackages
        , "sources"  Aeson..= taSources
        ]
