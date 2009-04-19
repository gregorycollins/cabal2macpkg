-- |
-- Module    : cabal2macpkg: convert cabal packages to OSX package format
-- Copyright : (c) Gregory D. Collins, 2008-2009
-- License   : BSD3
-- Maintainer: greg@gregorycollins.net
--
-- Loosely based on cabal2arch by Don Stewart.
--
-- Rough outline of the process:
--
--   1. find a .cabal file in the current working directory
--
--   2. run @cabal build; cabal haddock@ into a staging area
--
--   3. run @cabal register --gen-script@ to generate a registration
--      script that will be run by the OS X installer
--
--   4. turn the staging area into a mac package file using the OS X
--      developer tools
--
-- A consequence of this quick n' dirty approach is that in order to
-- build the installer for a cabal package, you need to have already
-- installed all of its dependencies on the build machine.
------------------------------------------------------------------------

module Main (
  -- * Program entry point
    main
  , runMakePackage
 ) where


import Control.Exception
import Control.Monad

import System.IO


------------------------------------------------------------------------
-- local imports
import Program.MakePackage
import Program.MakeMetaPackage
import Program.Options
import Program.Util

------------------------------------------------------------------------
-- | Program entry point. Parses command line options, creates a
-- scratch directory, runs the package building process, and cleans up
-- after itself.
------------------------------------------------------------------------
main :: IO ()
main = do
  opts <- getOptions
  bracket getTempDirectory
          cleanupTempDirectory
          (runMain opts)


runMain :: Options -> FilePath -> IO ()
runMain opts tempdir =
  if areBuildingMetaPackage opts then
      runMakeMetaPkg opts tempdir
    else
      runMakePackage opts tempdir
