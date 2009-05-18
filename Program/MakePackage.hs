-- | This module contains routines for making mac .pkg files.
------------------------------------------------------------------------
module Program.MakePackage ( runMakePackage ) where

import Control.Monad

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Version

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Simple.Utils hiding (intercalate)
import Distribution.Verbosity as Verbosity

import System.Directory
import System.FilePath
import System.IO

import Text.Regex


------------------------------------------------------------------------
-- local imports
import Distribution.OSX.PackageInfo
import Program.Options
import Program.Util


------------------------------------------------------------------------
-- | The program driver. Given the command-line options and a temp
-- directory path, searches the current working directory for a .cabal
-- file and builds an OSX package file based on its contents.
------------------------------------------------------------------------
runMakePackage :: Options       -- ^ command-line options
               -> FilePath      -- ^ temp directory path
               -> IO ()
runMakePackage opts tmpdir = do
  cabalFile <- findPackageDesc "."
  pkgDesc   <- flattenPackageDescription `liftM`
                 readPackageDescription Verbosity.normal cabalFile

  makeMacPkg opts tmpdir pkgDesc


------------------------------------------------------------------------
-- "the guts"
------------------------------------------------------------------------

------------------------------------------------------------------------
-- | the 'makeMacPkg' function does (or will do) all of the dirty work
-- of building the .pkg files
makeMacPkg :: Options            -- ^ command-line options
           -> FilePath           -- ^ path to temp directory
           -> PackageDescription -- ^ a parsed .cabal file
           -> IO ()
makeMacPkg opts tmpdir pkgDesc = do
    -- some portions of package building process require root
    -- privileges (thanks, Apple!)
    checkRootPrivileges
    createDirectories

    --------------------------------------------------------------------
    buildPackageContents
    setRootPrivileges
    mkInfoFiles
    runPackageMaker

  where
    --------------------------------------------------------------------
    -- variables
    --------------------------------------------------------------------

    -- package metadata
    pkgDescription       = synopsis pkgDesc
    pkgTitle             = unPackageName . packageName $ pkgDesc
    pkgVersionString     = showVersion . packageVersion $ pkgDesc
    pkgBaseName          = subRegex (mkRegex "[[:space:]]+") pkgTitle "_"

    -- directories
    cabalBuildDir        = tmpdir     </> "dist"
    contentsDir          = stagingDir </> "Contents"
    scriptsDir           = tmpdir     </> "Scripts"
    stagingDir           = tmpdir     </> "stage"

    -- config options
    packageMakerCmd      = fromJust $ packageMakerPath opts
    prefix               = fromJust $ installPrefix opts

    -- output files
    temporaryPkgConfig   = tmpdir      </> "temp.pkgconfig"
    infoPath             = tmpdir      </> "PackageInfo"
    postflightScriptFile = scriptsDir  </> "postflight"


    outputPackageDir     = fromMaybe "." (packageOutputDir opts)
    computedPackageFile  = (pkgBaseName ++ "-" ++ pkgVersionString ++ ".pkg")
    outputPackagePath    = fromMaybe (outputPackageDir </> computedPackageFile)
                                     (packageOutputFile opts)


    --------------------------------------------------------------------
    -- helper I/O actions
    --------------------------------------------------------------------


    --------------------------------------------------------------------
    -- creates necessary directories inside the work area
    createDirectories =
      (createDirectoryIfMissing True)
        `mapM_` [stagingDir, scriptsDir, contentsDir]


    --------------------------------------------------------------------
    -- uses cabal to build the package into the work area
    buildPackageContents = do
        runSetup   "configure" ["--global", "--prefix="++prefix]
        runSetup   "build"     []
        runSetup   "haddock"   []
        runSetup   "copy"      ["--destdir=" ++ contentsDir]
        runSetup   "register"  ["--gen-pkg-config=" ++ temporaryPkgConfig]

        makePostFlightScriptFile temporaryPkgConfig postflightScriptFile


    --------------------------------------------------------------------
    -- FIXME: make this stuff relocatable
    makePostFlightScriptFile src dest = do
        fe <- doesFileExist src
        if not fe then
            return ()
          else do
            contents <- readFile src
            let output = "#!/bin/sh\n\
                         \echo '" ++ contents ++ 
                          "' | /usr/bin/env ghc-pkg --global update -"
            writeFile dest output


    --------------------------------------------------------------------
    -- populate the packageinfo file in the resource directory
    mkInfoFiles :: IO ()
    mkInfoFiles = do
        nf <- getNumFiles contentsDir
        kb <- getFileSizesInKB contentsDir
        let pinfo = PackageInfo kb nf ("haskell."++pkgTitle)
                                Nothing (Just "postinstall")

        writePackageInfo infoPath pinfo


    --------------------------------------------------------------------
    -- make sure files are owned by root and have correct permissions
    setRootPrivileges :: IO ()
    setRootPrivileges = do
        runCmd "chmod" ["-R", "g+r,g-w,o+r,o-w", tmpdir]
        runCmd "chown" ["-R", "root:wheel", tmpdir]
        runCmd "sh" ["-c", "find " ++ tmpdir
                            ++ " -type d -print0 | xargs -0 chmod a+x"]


    --------------------------------------------------------------------
    -- build the package
    runPackageMaker :: IO ()
    runPackageMaker = do
        putStrLn $ "building " ++ outputPackagePath
        hFlush stdout

        runCmd packageMakerCmd [ "-o"
                               , outputPackagePath
                               , "--root"
                               , contentsDir
                               , "--scripts"
                               , scriptsDir
                               , "--target"
                               , "10.5"
                               , "--root-volume-only"
                               , "--info"
                               , infoPath ]



    --------------------------------------------------------------------
    -- helper functions
    --------------------------------------------------------------------

    -- | runs Setup.[l]hs with the given subcommand and arguments
    runSetup :: String          -- ^ subcommand of Setup.hs,
                                -- i.e. "configure"/"build"/etc
             -> [String]        -- ^ additional arguments
             -> IO ()
    runSetup cmd args =
        runCmd "runghc" $ ["Setup", cmd] ++ mkOpts args
      where
        mkOpts s = s ++ ["--builddir=" ++ cabalBuildDir]


    unPackageName (PackageName s) = s


