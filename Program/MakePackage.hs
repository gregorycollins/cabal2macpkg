-- | This module contains routines for making mac .pkg files.
------------------------------------------------------------------------
module Program.MakePackage ( makeMacPkg ) where

import Control.Monad

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Version

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils hiding (intercalate)

import System.Directory
import System.FilePath
import System.IO
import System.Posix.User (getEffectiveUserName)

import Text.Regex


------------------------------------------------------------------------
-- local imports
import Distribution.OSX.Info
import Program.Options
import Program.Util


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
    resourceDir          = tmpdir     </> "Resources"
    scriptsDir           = tmpdir     </> "Scripts"
    stagingDir           = tmpdir     </> "stage"

    -- config options
    packageMakerCmd      = fromJust $ packageMakerPath opts
    prefix               = fromJust $ installPrefix opts

    -- output files
    temporaryPkgConfig   = tmpdir      </> "temp.pkgconfig"
    infoPath             = tmpdir      </> "Info.plist"
    descInfoPath         = resourceDir </> "Description.plist"
    postflightScriptFile = resourceDir </> "postflight"


    outputPackageDir     = fromMaybe "." (packageOutputDir opts)
    computedPackageFile  = (pkgBaseName ++ "-" ++ pkgVersionString ++ ".pkg")
    outputPackagePath    = fromMaybe (outputPackageDir </> computedPackageFile)
                                     (packageOutputFile opts)


    --------------------------------------------------------------------
    -- helper I/O actions
    --------------------------------------------------------------------

    --------------------------------------------------------------------
    -- checks that we're root and bails if not
    checkRootPrivileges :: IO ()
    checkRootPrivileges = do
      whoiam <- getEffectiveUserName
      when (whoiam /= "root") $ die "must be root to run cabal2macpkg"


    --------------------------------------------------------------------
    -- creates necessary directories inside the work area
    createDirectories =
      createDirectory `mapM_` [stagingDir, scriptsDir, resourceDir,
                                         contentsDir]


    --------------------------------------------------------------------
    -- uses cabal to build the package into the work area
    buildPackageContents = do
        runSetup   "configure" ["--global", "--prefix=/usr/local"]
        runSetup   "build"     []
        runSetup   "haddock"   []
        runSetup   "copy"      ["--destdir=" ++ contentsDir]
        runSetup   "register"  ["--gen-pkg-config=" ++ temporaryPkgConfig]

        makePostFlightScriptFile temporaryPkgConfig postflightScriptFile


    --------------------------------------------------------------------
    -- FIXME: make this stuff relocatable
    makePostFlightScriptFile src dest = do
        contents <- readFile src
        let output = "#!/bin/sh\n\
                     \echo '" ++ contents ++ 
                     "' | /usr/bin/env ghc-pkg --global update -"
        writeFile dest output


    --------------------------------------------------------------------
    -- populate the package .info file in the resource directory
    mkInfoFiles :: IO ()
    mkInfoFiles = do
        writeFile infoPath (show pinfo)
        writeFile descInfoPath (show dpinfo)
      where
        pinfo = mkInfoPlist pkgBaseName
                            pkgVersionString
                            pkgDescription
                            prefix

        dpinfo = mkDescriptionPlist pkgBaseName pkgVersionString



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

        runCmd packageMakerCmd [ "-build"
                               , "-p"
                               , outputPackagePath
                               , "-f"
                               , contentsDir
                               , "-ds"
                               , "-r"
                               , resourceDir
                               , "-i"
                               , infoPath
                               , "-d"
                               , descInfoPath ]



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


