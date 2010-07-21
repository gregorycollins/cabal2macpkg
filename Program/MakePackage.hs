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
    hasPostFlight <- buildPackageContents
    setRootPrivileges
    mkInfoFiles hasPostFlight
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
        runSetup   "configure" [ "--global", "--prefix="++prefix, "-p"
                               , "--ld-options=-macosx_version_min 10.5" ]
        runSetup   "build"     []
        runSetup   "haddock"   ["--hyperlink-source"]
        runSetup   "copy"      ["--destdir=" ++ contentsDir]
        runSetup   "register"  ["--gen-pkg-config=" ++ temporaryPkgConfig]

        makePostFlightScriptFile temporaryPkgConfig postflightScriptFile


    --------------------------------------------------------------------
    -- FIXME: make this stuff relocatable
    makePostFlightScriptFile src dest = do
        fe <- doesFileExist src
        if not fe then
            return False
          else do
            contents <- readFile src
            let output = "#!/bin/sh\n\
                         \/usr/bin/ghc-pkg --global update - <<EOF\n"
                         ++ contents ++ "\nEOF\n"
            writeFile dest output
            return True


    --------------------------------------------------------------------
    -- populate the packageinfo file in the resource directory
    mkInfoFiles :: Bool -> IO ()
    mkInfoFiles hasPf = do
        nf <- getNumFiles contentsDir
        kb <- getFileSizesInKB contentsDir

        let pf = if hasPf then Just "postflight" else Nothing

        let pinfo = PackageInfo kb nf ("haskell."++pkgTitle)
                                Nothing pf

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
                               , "--id"
                               , ("haskell." ++ pkgTitle)
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
        runCmd "cabal" $ [cmd] ++ mkOpts args
      where
        mkOpts s = s ++ ["--builddir=" ++ cabalBuildDir]


    unPackageName (PackageName s) = s


