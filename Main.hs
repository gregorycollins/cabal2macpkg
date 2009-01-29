-- |
-- Module    : cabal2macpkg: convert cabal packages to OSX package format
-- Copyright : (c) Gregory D. Collins, 2008
-- License   : BSD3
--
-- Maintainer: greg@gregorycollins.net
-- Stability : early stage project
--
-- Loosely based on cabal2arch by Don Stewart.
--
-- Rough outline of the process:
--
-- 1) find a .cabal file in the current working directory
-- 2) run "cabal build; cabal haddock" into a staging area
-- 3) run "cabal register --gen-script" to generate a registration
--    script that will be run by the OS X installer
-- 4) turn the staging area into a mac package file using the OS X
--    developer tools
--
-- A consequence of this quick n' dirty approach is that in order to
-- build the installer for a cabal package, you need to have already
-- installed all of its dependencies on the build machine.


import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Simple.Build
import Distribution.Simple.Configure
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity as Verbosity

import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Concurrent
import Control.Exception

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Char
import Debug.Trace

import qualified System.Console.GetOpt as GetOpt

import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO
import System.Process

import System.Posix.User (getEffectiveUserName)



main :: IO ()
main = do
  options <- getOptions
  bracket getTempDirectory
          cleanupTempDirectory
          (runMain options)



runMain :: Options -> String -> IO ()
runMain options tmpdir = do
  findPackageDesc "." >>= makeMacPkg options tmpdir


------------------------------------------------------------------------
-- mac packageinfo files
------------------------------------------------------------------------
type PackageInfo = Map.Map String String


-- I stole this stuff from some python code, documentation for the
-- various file formats is difficult to find 
packageInfoFields = [
    "Title"
  , "Version"
  , "Description"
  , "DefaultLocation"
  , "DeleteWarning"
  , "NeedsAuthorization"
  , "DisableStop"
  , "UseUserMask"
  , "Application"
  , "Relocatable"
  , "Required"
  , "InstallOnly"
  , "RequiresReboot"
  , "RootVolumeOnly"
  , "LongFilenames"
  , "LibrarySubdirectory"
  , "AllowBackRev"
  , "OverwritePermissions"
  , "InstallFat"]


packageInfoDefaults :: PackageInfo
packageInfoDefaults = Map.fromList [
                        ("Title"                , ""        )
                      , ("Version"              , ""        )
                      , ("Description"          , ""        )
                      , ("DefaultLocation"      , "/"       )
                      , ("DeleteWarning"        , ""        )
                      , ("NeedsAuthorization"   , "NO"      )
                      , ("DisableStop"          , "NO"      )
                      , ("UseUserMask"          , "YES"     )
                      , ("Application"          , "NO"      )
                      , ("Relocatable"          , "YES"     )
                      , ("Required"             , "NO"      )
                      , ("InstallOnly"          , "NO"      )
                      , ("RequiresReboot"       , "NO"      )
                      , ("RootVolumeOnly"       , "NO"      )
                      , ("InstallFat"           , "NO"      )
                      , ("LongFilenames"        , "YES"     )
                      , ("LibrarySubdirectory"  , "Standard")
                      , ("AllowBackRev"         , "YES"     )
                      , ("OverwritePermissions" , "NO"      )
                      ]


packageInfoToString :: PackageInfo -> String
packageInfoToString pkg = e `concatMap` alist
  where
    alist = Map.toAscList pkg
    e (k,v) = k ++ " " ++ v ++ "\n"


------------------------------------------------------------------------
-- program options
------------------------------------------------------------------------
data Options = Options {
      installPrefix    :: Maybe String,
      packageMakerPath :: Maybe String,
      showUsage        :: Bool
    } deriving (Eq, Show)


------------------------------------------------------------------------
instance Monoid Options where
    mempty = Options { installPrefix = Just "/usr/local"
                     , showUsage = False
                       -- TODO: allow packageMakerPath to be overridden
                     , packageMakerPath = Just "/Developer/usr/bin/packagemaker"
                     }
    a `mappend` b =
        Options {
            installPrefix = ip
          , packageMakerPath = pmp
          , showUsage = (showUsage a) || (showUsage b)
          }
      where
        ipa  = installPrefix a
        ipb  = installPrefix b

        pmpa = packageMakerPath a
        pmpb = packageMakerPath b

        ip  = if isJust ipb then ipb else ipa
        pmp = if isJust pmpb then pmpb else pmpa

------------------------------------------------------------------------
optionFlags = [ GetOpt.Option
                  ['h']
                  ["help"]
                  (GetOpt.NoArg $ mempty {showUsage=True})
                  "prints usage statement"
              , GetOpt.Option
                  []
                  ["prefix"]
                  (GetOpt.OptArg mkPrefix "DIR")
                  "installation prefix directory" ]
  where
    mkPrefix :: Maybe String -> Options
    mkPrefix m = mempty { installPrefix = m }


------------------------------------------------------------------------
usage :: [String] -> IO a
usage errs = do
  putStrLn $ (usageString header) ++ errstr
  exitFailure

  where
    usageString :: String -> String
    usageString = flip GetOpt.usageInfo $ optionFlags

    preamble =
        "cabal2macpkg is a tool to create OSX installer packages\
        \ for cabal libraries"

    usageLine = "Usage: cabal2macpkg [OPTION..]"

    header = preamble ++ "\n\n" ++ usageLine
                      ++ "\n" ++ (const '-' `map` usageLine) ++ "\n"

    errstr = if null errs then ""
             else "\n" ++ (concat errs)


getOptions :: IO Options
getOptions = do
  args <- getArgs
  opts <-
      case GetOpt.getOpt GetOpt.RequireOrder optionFlags args of
        (o,n,[])   -> return $ mconcat o
        (_,_,errs) -> usage errs

  if showUsage opts then usage [] else return opts

------------------------------------------------------------------------
-- end options stuff
------------------------------------------------------------------------



------------------------------------------------------------------------
-- TODO: this function getting unwieldy, refactor
makeMacPkg :: Options -> FilePath -> FilePath -> IO ()
makeMacPkg options tmpdir cabalFile = do
    -- some portions of package building process require root
    -- privileges
    whoiam <- getEffectiveUserName

    if whoiam /= "root" then
        die "must be root to run cabal2macpkg"
      else
        return ()

    -- build into our temporary directory
    createDirectory stagingDir
    createDirectory scriptsDir
    createDirectory contentsDir

    putStrLn $ "found a cabal file at '" ++ cabalFile ++ "'"

    pkgDesc <- flattenPackageDescription `liftM`
                 (readPackageDescription Verbosity.normal cabalFile)

    let packageVersion = "0.0-FIXME"

    --------------------------------------------------------------------
    runSetup "configure" ["--global", "--prefix=" ++ prefix]
    runSetup "build"     []
    runSetup "haddock"   []
    runSetup "copy"      ["--destdir=" ++ contentsDir]
    runSetup "register"  ["--gen-script"]

    copyFile "register.sh" $ postflightScriptFile
    runCmd "chmod" ["+x", postflightScriptFile]
    removeFile "register.sh"

    -- TODO: setRootPrivileges will make sure the generated files have
    -- the correct username & permissions
    setRootPrivileges

    -- make .info file
    mkInfoFile



  where
    -- installPrefix guaranteed not nothing
    prefix = fromJust $ installPrefix options

    cabalBuildDir = tmpdir     </> "dist"
    scriptsDir    = tmpdir     </> "Resources"
    stagingDir    = tmpdir     </> "stage"
    contentsDir   = stagingDir </> "Contents"

    postflightScriptFile = scriptsDir </> "postflight"
    infoPath             = tmpdir     </> "package.info"


    runSetup :: String -> [String] -> IO ()
    runSetup cmd opts =
        runCmd "runghc" $ ["Setup", cmd] ++ (mkOpts opts)


    mkOpts s = s ++ ["--builddir=" ++ cabalBuildDir]


    packageMaker :: String
    -- note "packageMakerPath options" guaranteed not Nothing
    packageMaker = fromJust $ packageMakerPath options

    -- TODO: fill in package identifier based on contents of cabal file
    pkgIdentifier :: String
    pkgIdentifier = ""

    packageTitle :: String
    packageTitle = "FIXME"

    -- path to output file
    pkgDestinationFile :: FilePath
    pkgDestinationFile = tmpdir </> "FIXME.pkg"


    -- TODO: fill out info file here
    mkInfoFile :: IO ()
    mkInfoFile = return ()


    -- TODO: make sure files are owned by root and have correct
    -- permissions
    setRootPrivileges :: IO ()
    setRootPrivileges = return ()

------------------------------------------------------------------------
-- |
-- run a subprocess with the given arguments, ignoring the output. Die
-- if the program returns an error.
--
runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  e <- rawSystem cmd args
  case e of ExitSuccess -> return ()
            ExitFailure _ -> die $ "command failed: " ++
                                   cmd ++ " " ++ (Data.List.intercalate " " args)



------------------------------------------------------------------------
-- |
-- grab a temporary directory and change into it. Returns the path to
-- the new directory and the path to the old working directory.
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
-- |
-- removes the temporary directory and returns back to the previous
-- cwd
--
cleanupTempDirectory :: FilePath -> IO ()
cleanupTempDirectory dir = do
    --removeDirectoryRecursive dir
    putStrLn $ "temporary directory is '" ++ dir ++ "'"
    return ()


