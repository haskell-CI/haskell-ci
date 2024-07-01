{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module HaskellCI.Config (
    Config (..),
    configGrammar,
    emptyConfig,
    readConfigFile,
) where

import HaskellCI.Prelude

import qualified Data.ByteString               as BS
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.FieldGrammar     as C
import qualified Distribution.Fields           as C
import qualified Distribution.Parsec           as C

import HaskellCI.Config.ConstraintSet
import HaskellCI.Config.Empty
import HaskellCI.Config.Grammar
import HaskellCI.Config.Type
import HaskellCI.Config.Ubuntu
import HaskellCI.ParsecUtils

emptyConfig :: Config
emptyConfig = case runEG configGrammar of
    Left xs -> error $ "Required fields: " ++ show xs
    Right x -> postprocessConfig x

-------------------------------------------------------------------------------
-- Reading
-------------------------------------------------------------------------------

readConfigFile :: MonadIO m => FilePath -> m Config
readConfigFile = liftIO . readAndParseFile parseConfigFile

parseConfigFile :: [C.Field C.Position] -> C.ParseResult Config
parseConfigFile fields0 = do
    config <- C.parseFieldGrammar C.cabalSpecLatest fields configGrammar
    config' <- traverse parseSection $ concat sections
    return $ postprocessConfig $ foldl' (&) config config'
  where
    (fields, sections) = C.partitionFields fields0

    parseSection :: C.Section C.Position -> C.ParseResult (Config -> Config)
    parseSection (C.MkSection (C.Name pos name) args cfields)
        | name == "constraint-set" = do
            name' <- parseName pos args
            let (fs, _sections) = C.partitionFields cfields
            cs <- C.parseFieldGrammar C.cabalSpecLatest fs (constraintSetGrammar name')
            return $ over (field @"cfgConstraintSets") (cs :)
        | name == "raw-project" = do
            let fs = C.fromParsecFields cfields
            return $ over (field @"cfgRawProject") (++ map void fs)
        | otherwise = do
            C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ fromUTF8BS name
            return id

postprocessConfig :: Config -> Config
postprocessConfig cfg
    -- on yammy the only install option is ghcup
    | cfgUbuntu cfg >= Jammy = cfg { cfgGhcupJobs = anyVersion }
    | otherwise              = cfg

-------------------------------------------------------------------------------
-- From Cabal
-------------------------------------------------------------------------------

parseName :: C.Position -> [C.SectionArg C.Position] -> C.ParseResult String
parseName pos args = fromUTF8BS <$> parseNameBS pos args

parseNameBS :: C.Position -> [C.SectionArg C.Position] -> C.ParseResult BS.ByteString
parseNameBS pos args = case args of
    [C.SecArgName _pos secName] ->
         pure secName
    [C.SecArgStr _pos secName] ->
         pure secName
    [] -> do
         C.parseFailure pos "name required"
         pure ""
    _ -> do
         -- TODO: pretty print args
         C.parseFailure pos $ "Invalid name " ++ show args
         pure ""
