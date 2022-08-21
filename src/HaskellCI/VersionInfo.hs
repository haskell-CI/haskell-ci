{-# LANGUAGE CPP #-}
module HaskellCI.VersionInfo (
    haskellCIVerStr,
    dependencies,
) where

import HaskellCI.Prelude

import Data.Map          (Map)

import qualified Data.Map as Map

haskellCIVerStr :: String
haskellCIVerStr = VERSION_haskell_ci

dependencies :: Map String String
dependencies = Map.fromList
    [ ("aeson",                  VERSION_aeson)
    , ("base",                   VERSION_base)
    , ("base-compat",            VERSION_base_compat)
    , ("bytestring",             VERSION_bytestring)
    , ("Cabal-syntax",           VERSION_Cabal_syntax)
    , ("cabal-install-parsers",  VERSION_cabal_install_parsers)
    , ("containers",             VERSION_containers)
    , ("deepseq",                VERSION_deepseq)
    , ("directory",              VERSION_directory)
    , ("exceptions",             VERSION_exceptions)
    , ("filepath",               VERSION_filepath)
    , ("generic-lens-lite",      VERSION_generic_lens_lite)
    , ("HsYAML",                 VERSION_HsYAML)
    , ("lattices",               VERSION_lattices)
    , ("mtl",                    VERSION_mtl)
    , ("network-uri",            VERSION_network_uri)
    , ("optparse-applicative",   VERSION_optparse_applicative)
    , ("parsec",                 VERSION_parsec)
    , ("pretty",                 VERSION_pretty)
    , ("process",                VERSION_process)
#ifdef VERSION_ShellCheck
    , ("ShellCheck",             VERSION_ShellCheck)
#endif
    , ("temporary",              VERSION_temporary)
    , ("text",                   VERSION_text)
    , ("transformers",           VERSION_transformers)
    , ("unordered-containers",   VERSION_unordered_containers)
    ]
