-- Helpers to generate steps using tools: hlint and doctest
module HaskellCI.Tools (
    -- * Doctest
    doctestJobVersionRange,
    doctestArgs,
    ) where

import HaskellCI.Prelude

import qualified Distribution.PackageDescription               as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Distribution.Pretty                           as C
import qualified Distribution.Types.VersionRange               as C
import qualified Distribution.Utils.Path                       as C
import qualified Distribution.Version                          as C

import qualified Distribution.Types.BuildInfo.Lens          as L
import qualified Distribution.Types.PackageDescription.Lens as L

import HaskellCI.Compiler

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

doctestJobVersionRange :: CompilerRange
doctestJobVersionRange = RangeGHC /\ Range (C.orLaterVersion $ C.mkVersion [8,0])

-- | Modules arguments to the library
--
-- * We check the library component
--
-- * If there are hs-source-dirs, use them
--
-- * otherwise use exposed + other modules
--
-- * Also add default-extensions
--
doctestArgs :: C.GenericPackageDescription -> [[String]]
doctestArgs gpd = nub $
    [ libraryModuleArgs c
    | c <- toListOf (L.library . traverse) (C.flattenPackageDescription gpd)
    ] ++
    [ libraryModuleArgs c
    | c <- toListOf (L.subLibraries . traverse) (C.flattenPackageDescription gpd)
    ]

libraryModuleArgs :: C.Library -> [String]
libraryModuleArgs l
    | null dirsOrMods = []
    | otherwise       = lang ++ exts ++ dirsOrMods
  where
    bi = l ^. L.buildInfo

    dirsOrMods
        | null (C.hsSourceDirs bi) = map C.prettyShow (C.exposedModules l)
        | otherwise                = map C.getSymbolicPath $ C.hsSourceDirs bi

    lang = maybe [] (pure . ("-X" ++) . C.prettyShow) (C.defaultLanguage bi)

    exts = map (("-X" ++) . C.prettyShow) (C.defaultExtensions bi)
