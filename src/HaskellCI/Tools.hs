-- Helpers to generate steps using tools: hlint and doctest
module HaskellCI.Tools (
    -- * Doctest
    doctestJobVersionRange,
    doctestArgs,
    -- * HLint
    hlintJobVersionRange,
    hlintArgs,
    ) where

import Data.Generics.Labels ()
import Data.List            (nub)
import Data.Set             (Set)
import Lens.Micro

import qualified Data.Set                                      as S
import qualified Distribution.PackageDescription               as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Distribution.Pretty                           as C
import qualified Distribution.Types.VersionRange               as C
import qualified Distribution.Version                          as C

import qualified Distribution.Types.BuildInfo.Lens          as L
import qualified Distribution.Types.PackageDescription.Lens as L

import HaskellCI.Config.HLint

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

doctestJobVersionRange :: C.VersionRange
doctestJobVersionRange = C.orLaterVersion $ C.mkVersion [8,0]

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
-- /Note:/ same argument work for hlint too, but not exactly
--
doctestArgs :: C.GenericPackageDescription -> [[String]]
doctestArgs gpd = nub $
    [ libraryModuleArgs c
    | c <- C.flattenPackageDescription gpd ^.. L.library . traverse
    ] ++
    [ libraryModuleArgs c
    | c <- C.flattenPackageDescription gpd ^.. L.subLibraries . traverse
    ]

libraryModuleArgs :: C.Library -> [String]
libraryModuleArgs l
    | null dirsOrMods = []
    | otherwise       = exts ++ dirsOrMods
  where
    bi = l ^. L.buildInfo

    dirsOrMods
        | null (C.hsSourceDirs bi) = map C.prettyShow (C.exposedModules l)
        | otherwise                 = C.hsSourceDirs bi

    exts = map (("-X" ++) . C.prettyShow) (C.defaultExtensions bi)

executableModuleArgs :: C.Executable -> [String]
executableModuleArgs e
    | null dirsOrMods = []
    | otherwise       = exts ++ dirsOrMods
  where
    bi = e ^. L.buildInfo

    dirsOrMods
        -- note: we don't try to find main_is location, if hsSourceDirs is empty.
        | null (C.hsSourceDirs bi) = map C.prettyShow (C.otherModules bi)
        | otherwise                 = C.hsSourceDirs bi

    exts = map (("-X" ++) . C.prettyShow) (C.defaultExtensions bi)

-------------------------------------------------------------------------------
-- HLint
-------------------------------------------------------------------------------

hlintJobVersionRange :: Set (Maybe C.Version) -> HLintJob -> C.VersionRange
hlintJobVersionRange vs HLintJobLatest = case S.maxView vs of
    Just (Just v, _) -> C.thisVersion v
    _                -> C.thisVersion $ C.mkVersion [8,6,3]
hlintJobVersionRange _ (HLintJob v)   = C.thisVersion v

hlintArgs :: C.GenericPackageDescription -> [[String]]
hlintArgs gpd = nub $
    [ libraryModuleArgs c
    | c <- C.flattenPackageDescription gpd ^.. L.library . traverse
    ] ++
    [ libraryModuleArgs c
    | c <- C.flattenPackageDescription gpd ^.. L.subLibraries . traverse
    ] ++
    [ executableModuleArgs c
    | c <- C.flattenPackageDescription gpd ^.. L.executables . traverse
    ]
