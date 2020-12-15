{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module HaskellCI.GitHub.Yaml where

import HaskellCI.Prelude

import qualified Data.Map.Strict as M

import HaskellCI.List
import HaskellCI.YamlSyntax
import HaskellCI.Sh

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data GitHub = GitHub
    { ghOn   :: GitHubOn
    , ghJobs :: M.Map String GitHubJob
    }
  deriving (Show)

newtype GitHubOn = GitHubOn
    { ghBranches :: [String]
    }
  deriving (Show)

data GitHubJob = GitHubJob
    { ghjName      :: String
    , ghjRunsOn    :: String
    , ghjContainer :: Maybe String
    , ghjMatrix    :: [M.Map String String]
    , ghjSteps     :: [GitHubStep]
    }
  deriving (Show)

data GitHubStep = GitHubStep
    { ghsName :: String
    , ghsStep :: Either GitHubRun GitHubUses
    }
  deriving (Show)

-- | Steps with @run@
data GitHubRun = GitHubRun
    { ghsRun :: [Sh]
    , ghsEnv :: M.Map String String
    }
  deriving (Show)

-- | Steps with @uses@
data GitHubUses = GitHubUses
    { ghsAction :: String
    , ghsWith   :: M.Map String String
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- ToYaml
-------------------------------------------------------------------------------

instance ToYaml GitHub where
    toYaml GitHub {..} = ykeyValuesFilt []
        [ "name" ~> fromString "Haskell-CI"
        , "on"   ~> toYaml ghOn
        , "jobs" ~> ykeyValuesFilt []
            [ ([], j, toYaml job)
            | (j, job) <- M.toList ghJobs
            ]
        ]

instance ToYaml GitHubOn where
    toYaml GitHubOn {..}
        | null ghBranches
        = ylistFilt [] ["push", "pull_request"]
        | otherwise
        = ykeyValuesFilt []
              [ "push"         ~> branches
              , "pull_request" ~> branches
              ]
      where
        branches = ykeyValuesFilt []
            [ "branches" ~> ylistFilt [] (map fromString ghBranches)
            ]

instance ToYaml GitHubJob where
    toYaml GitHubJob {..} = ykeyValuesFilt []
        [ "name" ~> fromString ghjName
        , "runs-on" ~> fromString ghjRunsOn
        , "container" ~> ykeyValuesFilt [] (buildList $
            for_ ghjContainer $ \image -> item $ "image" ~> fromString image)
        , "strategy" ~> ykeyValuesFilt []
            [ "matrix" ~> ykeyValuesFilt []
                [ "include" ~> ylistFilt []
                    [ mapToYaml entry
                    | entry <- ghjMatrix
                    ]
                ]
            , "fail-fast" ~> YBool [] False
            ]
        , "steps" ~> ylistFilt [] (map toYaml $ filter notEmptyStep ghjSteps)
        ]

instance ToYaml GitHubStep where
    toYaml GitHubStep {..} = ykeyValuesFilt [] $
        [ "name" ~> fromString ghsName
        ] ++ case ghsStep of
            Left GitHubRun {..} ->
                [ "run" ~> fromString (shlistToString ghsRun)
                , "env" ~> mapToYaml ghsEnv
                ]

            Right GitHubUses {..} ->
                [ "uses" ~> fromString ghsAction
                , "with" ~> mapToYaml ghsWith
                ]

notEmptyStep :: GitHubStep -> Bool
notEmptyStep (GitHubStep _ (Left (GitHubRun [] _))) = False
notEmptyStep _                                      = True

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mapToYaml :: M.Map String String -> Yaml [String]
mapToYaml m = ykeyValuesFilt []
    [ k ~> fromString v
    | (k, v) <- M.toList m
    ]

shlistToString :: [Sh] -> String
shlistToString shs = unlines (map go shs) where
    go (Comment c) = "# " ++ c
    go (Sh x)      = x
