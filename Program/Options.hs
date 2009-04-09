-- | Datatypes for handling cabal2macpkg command-line options

module Program.Options
(
   Options(..)
  , getOptions
  , optionFlags
  , usage
) where

import Control.Monad

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

import System.Environment (getArgs)
import System.Exit
import System.IO

import qualified System.Console.GetOpt as GetOpt


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
      installPrefix    :: Maybe String   -- ^ the installation prefix
                                         -- for the generated library

    , packageMakerPath :: Maybe String   -- ^ path to the OSX
                                         -- packagemaker binary (we'll
                                         -- choose a sane default
                                         -- here)

    , showUsage        :: Bool           -- ^ if true, show the usage
                                         -- message, either because
                                         -- the user requested it or
                                         -- because of an error
                                         -- parsing the command line
                                         -- arguments

    , packageOutputDir :: Maybe String   -- ^ output dir for generated
                                         -- .pkg file

    , packageOutputFile :: Maybe String  -- ^ output filename for
                                         -- generated .pkg file -- if
                                         -- specified overrides
                                         -- packageOutputDir

    } deriving (Eq, Show)


defaultOptions :: Options
defaultOptions = Options { installPrefix     = Just "/"
                         , showUsage         = False
                         , packageMakerPath  = Just "/Developer/usr/bin/packagemaker"
                         , packageOutputDir  = Nothing
                         , packageOutputFile = Nothing
                         }

instance Monoid Options where
    mempty = Options { installPrefix     = Nothing
                     , showUsage         = False
                     , packageMakerPath  = Nothing
                     , packageOutputDir  = Nothing
                     , packageOutputFile = Nothing
                     }

    a `mappend` b =
        Options {
              installPrefix     = override installPrefix
            , packageMakerPath  = override packageMakerPath
            , packageOutputDir  = override packageOutputDir
            , packageOutputFile = override packageOutputFile
            , showUsage         = showUsage a || showUsage b
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
                  "installation prefix directory"

              , GetOpt.Option
                  "d"
                  ["outdir"]
                  (GetOpt.OptArg mkOutputDir "DIR")
                  "output install package to the given directory (default \".\")"

              , GetOpt.Option
                  "o"
                  ["output"]
                  (GetOpt.OptArg mkOutputFile "FILE")
                  "output install package to the given file"
              ]

  where
    mkPrefix :: Maybe String -> Options
    mkPrefix m = mempty { installPrefix = m }

    mkOutputDir :: Maybe String -> Options
    mkOutputDir m = mempty { packageOutputDir = m }

    mkOutputFile :: Maybe String -> Options
    mkOutputFile m = mempty { packageOutputFile = m }


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


