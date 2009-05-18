-- | Utility functions
module Program.Util
  ( runCmd
  , runCmdWithOutput
  , getFileSizesInKB
  , getNumFiles
  , getTempDirectory
  , cleanupTempDirectory
  , checkRootPrivileges
  ) where



import Control.Monad

import Data.Char
import Data.Function
import Data.List

import Distribution.Simple.Utils hiding (intercalate)

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.User (getEffectiveUserName)
import System.Process


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


runCmdWithOutput :: String
                 -> IO String
runCmdWithOutput cmd = do
  readProcess "sh" ["-c", cmd] ""


getNumFiles :: FilePath -> IO Int
getNumFiles fp =
    runCmdWithOutput ("find '" ++ fp ++ "' -type f | wc -l")
        >>= return . (read :: String -> Int)

getFileSizesInKB :: FilePath -> IO Int
getFileSizesInKB fp = do
    txt <- runCmdWithOutput ("du -kd0 " ++ fp)
    let (n,_) = span isDigit txt
    return $ read n


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
cleanupTempDirectory = removeDirectoryRecursive


------------------------------------------------------------------------
-- | checks that we're root and bails if not
checkRootPrivileges :: IO ()
checkRootPrivileges = do
  whoiam <- getEffectiveUserName
  when (whoiam /= "root") $ die "must be root to run cabal2macpkg"


