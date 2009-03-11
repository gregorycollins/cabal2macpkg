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
  , runMain

  -- * Command-line options
  , Options(..)
  , getOptions
  , optionFlags
  , usage

  -- * The \"heavy lifting\"
  , makeMacPkg

  -- * Misc. helper functions
  , cleanupTempDirectory 
  , getTempDirectory
  , runCmd
 ) where


import Control.Exception
import Control.Monad

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Version

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Simple.Utils hiding (intercalate)
import Distribution.Verbosity as Verbosity

import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO
import System.Posix.User (getEffectiveUserName)
import System.Process

import Text.Regex

import qualified System.Console.GetOpt as GetOpt


------------------------------------------------------------------------
-- local imports
import Distribution.OSX.Info


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



------------------------------------------------------------------------
-- | The guts of the program. Given the command-line options and a
-- temp directory path, searches the current working directory for a
-- .cabal file and builds an OSX package file based on its contents.
------------------------------------------------------------------------
runMain :: Options              -- ^ command-line options
        -> FilePath             -- ^ temp directory path
        -> IO ()
runMain opts tmpdir = do
  cabalFile <- findPackageDesc "."
  pkgDesc   <- flattenPackageDescription `liftM`
                 readPackageDescription Verbosity.normal cabalFile

  makeMacPkg opts tmpdir pkgDesc



------------------------------------------------------------------------
-- program options
------------------------------------------------------------------------

------------------------------------------------------------------------
-- | A monoid instance for the command-line options allows us to build
-- up the options object from parts, i.e.:
-- @
--   a \``mappend`\` b
-- @
-- will build an Options object where the fields of @a@ are overridden
-- by the non-'Nothing' fields of @b@
data Options = Options {
      installPrefix    :: Maybe String  -- ^ the installation prefix
                                        -- for the generated library

    , packageMakerPath :: Maybe String  -- ^ path to the OSX
                                        -- packagemaker binary (we'll
                                        -- choose a sane default here)

    , showUsage        :: Bool          -- ^ if true, show the usage
                                        -- message, either because the
                                        -- user requested it or
                                        -- because of an error parsing
                                        -- the command line arguments
    } deriving (Eq, Show)


defaultOptions :: Options
defaultOptions = Options { installPrefix    = Just "/usr/local"
                         , showUsage        = False
                         , packageMakerPath = Just "/Developer/usr/bin/packagemaker"
                         }

instance Monoid Options where
    mempty = Options { installPrefix    = Nothing
                     , showUsage        = False
                     , packageMakerPath = Nothing
                     }

    a `mappend` b =
        Options {
              installPrefix    = override installPrefix
            , packageMakerPath = override packageMakerPath
            , showUsage        = showUsage a || showUsage b
          }
      where
        -- monoid append using "Last" behaviour
        (*+*)    :: Maybe a -> Maybe a -> Maybe a
        (*+*)    = (getLast .) . (mappend `on` Last)

        override f = f a *+* f b


------------------------------------------------------------------------
-- | list of the option flags we accept (for GetOpt)
optionFlags :: [GetOpt.OptDescr Options]
optionFlags = [ GetOpt.Option
                  "h"
                  ["help"]
                  (GetOpt.NoArg $ mempty {showUsage=True})
                  "prints usage statement"
              , GetOpt.Option
                  ""
                  ["prefix"]
                  (GetOpt.OptArg mkPrefix "DIR")
                  "installation prefix directory" ]
  where
    mkPrefix :: Maybe String -> Options
    mkPrefix m = mempty { installPrefix = m }


------------------------------------------------------------------------
-- | prints the usage statement
usage :: [String]               -- ^ list of error messages
      -> IO a
usage errs = do
  putStrLn $ usageString header ++ errstr
  exitFailure

  where
    usageString :: String -> String
    usageString = flip GetOpt.usageInfo optionFlags

    preamble =
        "cabal2macpkg is a tool to create OSX installer packages\
        \ for cabal libraries"

    usageLine = "Usage: cabal2macpkg [OPTION..]"

    header = preamble ++ "\n\n" ++ usageLine
                      ++ "\n" ++ (const '-' `map` usageLine) ++ "\n"

    errstr = if null errs then ""
             else '\n' : concat errs


------------------------------------------------------------------------
-- | parses the command line arguments -- shows usage screen and bails
-- upon error
getOptions :: IO Options
getOptions = do
  args <- getArgs
  opts <-
      case GetOpt.getOpt GetOpt.RequireOrder optionFlags args of
        (o,_,[])   -> return $ defaultOptions `mappend` mconcat o
        (_,_,errs) -> usage errs

  if showUsage opts
    then usage []
    else return opts

------------------------------------------------------------------------
-- end options stuff
------------------------------------------------------------------------


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
    -- privileges

    -- TODO/FIXME: check that assumption, during development it isn't
    -- convenient so I'm commenting it out temporarily

    -- checkRootPrivileges


    createDirectories

    --------------------------------------------------------------------
    buildPackageContents

    -- TODO: setRootPrivileges will make sure the generated files have
    -- the correct username & permissions
    setRootPrivileges

    -- make .info files
    mkInfoFiles

  where
    --------------------------------------------------------------------
    -- variables
    --------------------------------------------------------------------

    -- package metadata
    pkgDescription       = synopsis pkgDesc
    pkgTitle             = unPackageName . packageName $ pkgDesc
    pkgVersionString     = showVersion . packageVersion $ pkgDesc
    pkgBaseName          = subRegex (mkRegex "[[:space:]]+") pkgTitle "_"
    pkgDestinationFile   = tmpdir </> (pkgBaseName ++ "-" ++ pkgVersionString
                                                   ++ ".pkg")

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
    infoPath             = tmpdir      </> "Info.plist"
    descInfoPath         = resourceDir </> "Description.plist"
    postflightScriptFile = resourceDir </> "postflight"


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
      runSetup   "configure" ["--global", "--prefix=/testme"]
      runSetup   "build"     []
      runSetup   "haddock"   []
      runSetup   "copy"      ["--destdir=" ++ contentsDir]
      runSetup   "register"  ["--gen-script"]

      copyFile   "register.sh" postflightScriptFile
      runCmd     "chmod" ["+x", postflightScriptFile]
      removeFile "register.sh"


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


    -- TODO: make sure files are owned by root and have correct
    -- permissions
    setRootPrivileges :: IO ()
    setRootPrivileges = return ()


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


------------------------------------------------------------------------
-- misc. useful functions
------------------------------------------------------------------------


------------------------------------------------------------------------
-- |
-- run a subprocess with the given arguments, ignoring the output. Die
-- if the program returns a nonzero status code
--
runCmd :: String                -- ^ command to run
       -> [String]              -- ^ command arguments
       -> IO ()
runCmd cmd args = do
  e <- rawSystem cmd args
  case e of ExitSuccess -> return ()
            ExitFailure _ -> die $ "command failed: "
                                   ++ cmd ++ " "
                                   ++ intercalate " " args



------------------------------------------------------------------------
-- | grab a temporary directory. Produces the path to the new
-- directory.
--
getTempDirectory :: IO FilePath
getTempDirectory =
    do (ecode, out, err) <- readProcessWithExitCode
                              "mktemp"
                              ["-d", "-t", "cabal2macpkg"] []

       case ecode of
         ExitSuccess -> do
                   let dir = makeValid (init out)
                   return dir
         ExitFailure _ -> die $ "mktemp failed, saying '" ++ err ++ "'"



------------------------------------------------------------------------
-- | cleans up a temporary directory
--
cleanupTempDirectory :: FilePath
                     -> IO ()
cleanupTempDirectory dir = do
    --removeDirectoryRecursive dir
    putStrLn $ "temporary directory is '" ++ dir ++ "'"
    return ()


