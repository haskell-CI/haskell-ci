{-# LANGUAGE OverloadedStrings #-}
module HaskellCI.GitConfig (
    GitConfig (..),
    emptyGitConfig,
    readGitConfig,
) where

import HaskellCI.Prelude

import Data.Ini

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict      as Map

data GitConfig = GitConfig
    { gitCfgRemotes :: Map.Map Text Text
    }
  deriving Show

emptyGitConfig :: GitConfig
emptyGitConfig = GitConfig
    { gitCfgRemotes = mempty
    }

-- | Read 'GitConfig'. On error, return 'emptyGitConfg'.
readGitConfig :: IO GitConfig
readGitConfig = do
    e <- readIniFile ".git/config"
    return $ case e of
        Left _    -> emptyGitConfig
        Right ini -> elaborateGitConfig ini

elaborateGitConfig :: Ini -> GitConfig
elaborateGitConfig ini = ifoldr go emptyGitConfig (iniSections ini) where
    go :: Text -> [(Text, Text)] -> GitConfig -> GitConfig
    go secname secfields cfg
        | Right name <- Atto.parseOnly (sectionP <* Atto.endOfInput) secname
        , Just url   <- lookup "url" secfields
        = cfg
            { gitCfgRemotes = Map.insert name url (gitCfgRemotes cfg)
            }

    go _ _ cfg = cfg

-- We use attoparsec here, because it backtracks
sectionP :: Atto.Parser Text
sectionP = do
    _ <- Atto.string "remote"
    Atto.skipSpace
    _ <- Atto.char '"'
    remote <- Atto.takeWhile (/= '"')
    _ <- Atto.char '"'
    return remote
    
