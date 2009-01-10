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
import Data.Maybe
import Data.Monoid
import Data.Char
import Debug.Trace

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import System.Posix.User (getEffectiveUserName)


main :: IO ()
main = do
  bracket getTempDirectory cleanupTempDirectory runMain


runMain :: String -> IO ()
runMain tmpdir = do
  findPackageDesc "." >>= makeMacPkg tmpdir


whoami :: IO String
whoami = getEffectiveUserName


makeMacPkg :: FilePath -> FilePath -> IO ()
makeMacPkg tmpdir cabalFile = do
    -- some portions of package building process require root
    -- privileges
    whoiam <- whoami

    if whoiam /= "root" then
        die "must be root to run cabal2macpkg"
      else
        return ()

    -- build into our temporary directory
    createDirectory stagingDir
    let buildFlags = defaultBuildFlags { buildDistPref = toFlag stagingDir }
    putStrLn $ "found a cabal file at '" ++ cabalFile ++ "'"
    
    -- pkgDesc <- flattenPackageDescription $
    --              readPackageDescription Verbosity.normal
    --                                     cabalFile
    
    pkgDesc <- flattenPackageDescription `liftM`
                 (readPackageDescription Verbosity.normal cabalFile)

    let prefix = "/Library/Frameworks/GHC.framework/Versions/Current/usr"
    
    runSetup "configure" ["--global", "--prefix=" ++ prefix]
    runSetup "build"     []
    runSetup "haddock"   []
    runSetup "copy"      ["--destdir=" ++ stagingDir]
    runSetup "register"  ["--gen-script"]

    copyFile "register.sh" $ tmpdir </> "register.sh"
    removeFile "register.sh"

    --setRootPrivileges 

    putStrLn $ "tmpdir is " ++ tmpdir
  where
    cabalBuildDir = tmpdir </> "dist"
    stagingDir = tmpdir </> "stage"

    runSetup :: String -> [String] -> IO ()
    runSetup cmd opts =
        runCmd "runghc" $ ["Setup", cmd] ++ (mkOpts opts)

    mkOpts s = s ++ ["--builddir=" ++ cabalBuildDir]


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


-- |
-- removes the temporary directory and returns back to the previous
-- cwd
--
cleanupTempDirectory :: FilePath -> IO ()
cleanupTempDirectory dir = do
    --removeDirectoryRecursive dir
    return ()

