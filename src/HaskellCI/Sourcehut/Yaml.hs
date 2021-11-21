{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
-- | @.builds@ structure.
module HaskellCI.Sourcehut.Yaml where

import HaskellCI.Prelude

import qualified Data.Map.Strict as M

import HaskellCI.Config.Ubuntu
import HaskellCI.Sh
import HaskellCI.YamlSyntax

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

newtype Sourcehut = Sourcehut
    { sourcehutManifests :: M.Map String SourcehutManifest
    }

data SourcehutManifest = SourcehutManifest
    { srhtManifestImage :: Ubuntu
    , srhtManifestPackages :: [String]
    , srhtManifestRepositories :: M.Map String String
    , srhtManifestArtifacts :: [FilePath]
    , srhtManifestSources :: [String]
    , srhtManifestTasks :: [SourcehutTask]
    , srhtManifestTriggers :: [SourcehutTrigger]
    , srhtManifestEnvironment :: M.Map String String
    }

data SourcehutTask = SourcehutTask String [Sh]

data SourcehutTrigger = SourcehutTriggerEmail String -- the "to" address
                      | SourcehutTriggerWebhook String

instance ToYaml SourcehutManifest where
    toYaml SourcehutManifest{..} = ykeyValuesFilt []
        [ "image" ~> fromString ("ubuntu/" ++ showUbuntu srhtManifestImage)
        , "packages" ~> ylistFilt [] (fromString <$> srhtManifestPackages)
        , "repositories" ~> ykeyValuesFilt []
            ((\(name, src) -> name ~> fromString src) <$> M.toList srhtManifestRepositories)
        , "artifacts" ~> ylistFilt [] (fromString <$> srhtManifestArtifacts)
        , "sources" ~> ylistFilt [] (fromString <$> srhtManifestSources)
        , "tasks" ~> ylistFilt []
            ((\(SourcehutTask name code) -> ykeyValuesFilt [] $ [name ~> fromString (shlistToString code)]) <$> srhtManifestTasks)
        , "triggers" ~> ylistFilt [] (toYaml <$> srhtManifestTriggers)
        , "environment" ~> ykeyValuesFilt []
            ((\(k, v) -> k ~> fromString v) <$> M.toList srhtManifestEnvironment)
        ]

instance ToYaml SourcehutTrigger where
    toYaml (SourcehutTriggerEmail to) = ykeyValuesFilt []
        [ "action" ~> "email"
        , "condition" ~> "failure"
        , "to" ~> fromString to
        ]
    toYaml (SourcehutTriggerWebhook url) = ykeyValuesFilt []
        [ "action" ~> "webhook"
        , "condition" ~> "failure"
        , "url" ~> fromString url
        ]
