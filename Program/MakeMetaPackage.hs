{-# LANGUAGE BangPatterns #-}

-- | This module contains routines for making a mac distribution file
-- from a .cabal file's dependencies. Note that for right now (until
-- this program develops further) the intention is to do just enough
-- to be able to build an installer for the Haskell Platform
------------------------------------------------------------------------

module Program.MakeMetaPackage (runMakeMetaPkg)
where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           Data.Version
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Utils hiding (intercalate)
import           Distribution.Verbosity as Verbosity
import           Distribution.Version
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FilePath.Glob
import           System.IO
import           Text.Printf
import           Text.Regex

------------------------------------------------------------------------
-- local imports
------------------------------------------------------------------------
import           Distribution.OSX.InstallerScript
import           Program.MakePackage
import           Program.Options
import           Program.Util


------------------------------------------------------------------------
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


------------------------------------------------------------------------
-- | Builds an OSX distribution based on a .cabal file
makeMacMetaPkg :: Options             -- ^ command-line options
               -> FilePath            -- ^ path to temp directory
               -> PackageDescription  -- ^ a parsed .cabal file
               -> IO ()
makeMacMetaPkg opts tmpdir pkgDesc = do
    cwd <- getCurrentDirectory
    checkDependenciesHaveExactVersions deps
    checkRootPrivileges

    outputPackageDir  <- makeAndCanonicalize $ fromMaybe cwd (packageOutputDir opts)
    outputPackagePath <- makeAndCanonicalize $
                         fromMaybe (outputPackageDir </> computedPackageFileName)
                                   (packageOutputFile opts)

    contentsDir       <- makeAndCanonicalize $ tmpdir </> "Stage"
    packagesDir       <- makeAndCanonicalize $ tmpdir </> "Packages"

    let subOptions = opts { packageOutputDir  = Just packagesDir
                          , packageOutputFile = Nothing }

    (createDirectoryIfMissing True) `mapM_` [ contentsDir
                                            , packagesDir ]

    mapM_ (buildOne subOptions) packagesToFetch

    -- if the user specified an extra packages directory, copy the extra
    -- packages to the output package directory and return a list of the
    -- filenames
    extraPackages <-
        maybe (return [])
              (\x -> do
                 files <- globPackages x
                 mapM_ (copyTo packagesDir) files
                 return (takeFileName `map` files))
              (extraPkgDir opts)


    let allPackages = extraPackages ++ packageFileNames

    -- FIXME: sizes bogus
    sizes <- mapM (unXarToStaging packagesDir contentsDir) allPackages

    -- write any Resources/ files (likely none), dump out
    -- "Distribution" file, and xar up the results

    -- FIXME: Resources/ (for background images)

    writeInstallerScript (contentsDir </> "Distribution") $
      installerScript pkgTitle
                      Nothing   -- FIXME: populate these
                      Nothing
                      (Just pkgDescription)
                      Nothing
                      Nothing
                      (allPackages `zip` sizes)

    xarUpResults contentsDir outputPackagePath

  where
    --------------------------------------------------------------------
    -- variables
    --------------------------------------------------------------------

    -- package metadata
    unPackageName (PackageName s) = s

    pkgDescription       = synopsis pkgDesc
    pkgTitle             = unPackageName . packageName $ pkgDesc
    pkgVersionString     = showVersion . packageVersion $ pkgDesc
    pkgBaseName          = subRegex (mkRegex "[[:space:]]+") pkgTitle "_"

    --------------------------------------------------------------------
    computedPackageFileName = (pkgBaseName ++ "-" ++ pkgVersionString ++ ".pkg")

    deps = executableDeps opts ++ buildDepends pkgDesc

    --------------------------------------------------------------------
    packagesToFetch :: [(String, String)]
    packagesToFetch = depToString `map` deps
      where
        depToString (Dependency (PackageName nm) (ThisVersion v)) =
            (nm, showVersion v)
        depToString _ = error "impossible"


    --------------------------------------------------------------------
    packageFileNames :: [String]
    packageFileNames = map (\(n,v) -> n ++ "-" ++ v ++ ".pkg")
                           packagesToFetch


    --------------------------------------------------------------------
    -- Actions
    --------------------------------------------------------------------
    unXarToStaging :: FilePath -> FilePath -> FilePath -> IO Int
    unXarToStaging pkgPath outDir pkgFile = do
        bracket
          getCurrentDirectory
          setCurrentDirectory
          (\_ -> do
             setCurrentDirectory outDir

             let srcFile = pkgPath </> pkgFile
             let destDir = outDir </> pkgFile

             createDirectoryIfMissing True $ destDir
             setCurrentDirectory $ destDir

             putStrLn $ printf "un-xaring '%s' to '%s'..." srcFile destDir
             putStrLn $ "------------------------------------------------------------------------"
             hFlush stdout
                              
             runCmd "xar" ["-xvf", srcFile]
             -- FIXME: parse PackageInfo
             return 0)


    --------------------------------------------------------------------
    xarUpResults :: FilePath -> FilePath -> IO ()
    xarUpResults staging outputFileName = do
        bracket
          getCurrentDirectory
          setCurrentDirectory
          (\_ -> do
             setCurrentDirectory staging
             runCmd "xar" ["-cvf", outputFileName, "."])


    --------------------------------------------------------------------
    cabalFetch (name,vers) = do
      -- FIXME: change this when cabal fetch takes an -o argument
      home <- getEnv "HOME"

      let pkgbase = name ++ "-" ++ vers
      let pkg     = pkgbase  ++ ".tar.gz"
      let pkgloc  = home </> ".cabal/packages/hackage.haskell.org/"
                         </> name </> vers </> pkg

      runCmd "cabal" ["fetch", pkgbase]
      fe <- doesFileExist pkgloc
      if fe then do
          runCmd "cp" [pkgloc, pkg]
          runCmd "tar" ["--strip-components=1", "-xvzf", pkg]

        else
          die $ "couldn't find file " ++ pkg


    --------------------------------------------------------------------
    buildOne opt (name,vers) = do
      putStrLn $ "\n" ++ (replicate 72 '-')
      putStrLn $ "Making " ++ name ++ "-" ++ vers
      putStrLn $ replicate 72 '-'
      hFlush   stdout

      bracket
        (liftM2 (,) getTempDirectory getCurrentDirectory)
        (\(x,y) -> cleanupTempDirectory x >> setCurrentDirectory y)
        (\(!td,_) -> do
            setCurrentDirectory td
            putStrLn $ "changed to " ++ td
            hFlush stdout

            let workdir = td </> "work"
            createDirectoryIfMissing True workdir
            cabalFetch (name,vers)
            runMakePackage opt workdir
            )

    --------------------------------------------------------------------






------------------------------------------------------------------------
-- | globs a directory for .pkg files
globPackages :: FilePath -> IO [FilePath]
globPackages dir = namesMatching `mapM` ((dir </>) `map` ["*.pkg"])
                     >>= return . concat


------------------------------------------------------------------------
-- | copy a file
copyTo :: FilePath -> FilePath -> IO ()
copyTo dest file = do
  isDir <- doesDirectoryExist dest
  let out = if isDir then dest </> takeFileName file else dest
  B.readFile file >>= B.writeFile out



------------------------------------------------------------------------
-- | The program driver. Given the command-line options and a temp
-- directory path, searches the current working directory for a .cabal
-- file and builds an OSX metapackage based on its dependencies.
------------------------------------------------------------------------
runMakeMetaPkg :: Options -> FilePath -> IO ()
runMakeMetaPkg opts tmpdir = do
  cabalFile <- findPackageDesc "."
  pkgDesc   <- flattenPackageDescription `liftM`
                 readPackageDescription Verbosity.normal cabalFile

  makeMacMetaPkg opts tmpdir pkgDesc



makeAndCanonicalize :: FilePath -> IO FilePath
makeAndCanonicalize fp = createDirectoryIfMissing True fp >> canonicalizePath fp
