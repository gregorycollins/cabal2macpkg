-- | This module contains routines for making a mac .mpkg file from a
-- .cabal file's dependencies. Note that for right now (until this
-- program develops further) the intention is to do just enough to be
-- able to build an installer for the Haskell Platform
------------------------------------------------------------------------

module Program.MakeMetaPackage where

import Control.Monad

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils hiding (intercalate)
import Distribution.Version


-- | Until "cabal fetch" has an "-o" argument there isn't really any
-- | way to get at the .tar.gz file if you don't know the exact
-- | version string
checkDependenciesHaveExactVersions :: [Dependency] -> IO ()
checkDependenciesHaveExactVersions d =
    when (not $ all isExact d)
         (die "all dependencies must be specified with exact versions")
  where
    isExact (Dependency _ (ThisVersion _)) = True
    isExact _                              = False
