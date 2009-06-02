{-# LANGUAGE NoMonomorphismRestriction #-}

module Distribution.OSX.InstallerScript
( installerScript
, writeInstallerScript
, installerScriptToString
, InstallerScript )
where

import Data.Maybe
import Distribution.PackageDescription.Configuration
import Text.XML.HXT.Arrow


------------------------------------------------------------------------
-- exports
------------------------------------------------------------------------

data InstallerScript = InstallerScript {
      is_title        :: String
    , is_background   :: Maybe String
    , is_welcome      :: Maybe String
    , is_readme       :: Maybe String
    , is_license      :: Maybe String
    , is_conclusion   :: Maybe String
    , is_pkgFileNames :: [(String,Int)]
}


------------------------------------------------------------------------
-- | Populate an InstallerScript object with the given values.
installerScript :: String           -- ^ package title
                -> Maybe String     -- ^ background image to use in the installer
                                    --   (FIXME: currently ignored)
                -> Maybe String     -- ^ welcome blurb
                -> Maybe String     -- ^ readme blurb
                -> Maybe String     -- ^ license blurb
                -> Maybe String     -- ^ conclusion blurb
                -> [(String,Int)]   -- ^ list of .pkg files to
                                    -- include, along with their
                                    -- installed sizes
                -> InstallerScript
installerScript = InstallerScript


------------------------------------------------------------------------
-- | Write a populated installer script to a file.
writeInstallerScript :: String          -- ^ file to write the output to
                     -> InstallerScript -- ^ the values for the installer script
                     -> IO ()
writeInstallerScript file is = runX ( mkInstallerScript is
                                      >>> 
                                      writeDocument [(a_indent, v_1)] file )
                               >> return ()


------------------------------------------------------------------------
-- | Render a populated installer script into a string.
installerScriptToString :: InstallerScript -> IO String
installerScriptToString is = runX ( mkInstallerScript is
                                    >>>
                                    writeDocumentToString [(a_indent, v_1)] )
                             >>= return . concat


------------------------------------------------------------------------
-- local functions
------------------------------------------------------------------------

------------------------------------------------------------------------
simpleTag :: (ArrowXml a) => String -> String -> a n XmlTree
simpleTag tagName text = mkelem tagName [] [txt text]


------------------------------------------------------------------------
mkTitle :: ArrowXml a => String -> a n XmlTree
mkTitle = simpleTag "title"


------------------------------------------------------------------------
mkOptions :: (ArrowXml a) => a n XmlTree
mkOptions = mkelem "options" [ sattr "customize" "never"
                             , sattr "allow-external-scripts" "no"
                             , sattr "rootVolumeOnly" "false"] []


------------------------------------------------------------------------
blurbAttrs :: (ArrowXml a) => [a n XmlTree]
blurbAttrs = [ sattr "language" "en"
             , sattr "mime-type" "text/plain" ]


------------------------------------------------------------------------
blurb :: (ArrowXml a) => String -> String -> a n XmlTree
blurb tagName s = mkelem tagName blurbAttrs [cdata s]


------------------------------------------------------------------------
mkReadme :: (ArrowXml a) => String -> a n XmlTree
mkReadme = blurb "readme"


------------------------------------------------------------------------
mkWelcome :: (ArrowXml a) => String -> a n XmlTree
mkWelcome = blurb "welcome"


------------------------------------------------------------------------
mkLicense :: (ArrowXml a) => String -> a n XmlTree
mkLicense = blurb "license"


------------------------------------------------------------------------
mkConclusion :: (ArrowXml a) => String -> a n XmlTree
mkConclusion = blurb "conclusion"


------------------------------------------------------------------------
cdata :: (ArrowXml cat) => String -> cat a XmlTree
cdata = (>>> mkCdata) . arr . const


------------------------------------------------------------------------
mkLine :: (ArrowXml a) => String -> a n XmlTree
mkLine choiceId = mkelem "line" [sattr "choice" choiceId] []


------------------------------------------------------------------------
mkChoicesOutline :: (ArrowXml a) => [String] -> a n XmlTree
mkChoicesOutline choiceIds =
    mkelem "choices-outline" [] (map mkLine choiceIds)


------------------------------------------------------------------------
mkChoice :: (ArrowXml a) => String -> String -> String -> a n XmlTree
mkChoice iD title pkgref =
    mkelem "choice"
           [ sattr "id"            iD
           , sattr "title"         title
           , sattr "start_visible" "false" ]
           [ mkelem "pkg-ref" [sattr "id" pkgref] [] ]


------------------------------------------------------------------------
mkPkgRef :: (ArrowXml a) => String -> String -> [Char] -> a n XmlTree
mkPkgRef iD installKBytes pkgFileName =
    mkelem "pkg-ref"
           [ sattr "id"            iD
           , sattr "installKBytes" installKBytes
           , sattr "version"       ""
           , sattr "auth"          "Root" ]
           [ txt $ "#" ++ pkgFileName ]


------------------------------------------------------------------------
installerScriptHead :: (ArrowXml a) => [a n XmlTree] -> a n XmlTree
installerScriptHead body =
    root [] [ mkelem "installer-script"
                     [ sattr "minSpecVersion" "1.000000" ]
                     body ]


------------------------------------------------------------------------
mkInstallerScript :: (ArrowXml a) => InstallerScript -> a n XmlTree
mkInstallerScript is =
    installerScriptHead $ concat [
                              [ mkTitle (is_title is) ]
                            , catMaybes [
                                  (is_welcome is)    >>= Just . mkWelcome
                                , (is_readme is)     >>= Just . mkReadme
                                , (is_license is)    >>= Just . mkLicense
                                , (is_conclusion is) >>= Just . mkConclusion ]
                            , [ choicesOutline ]
                            , choices
                            , pkgRefs ]
  where
    pkgFiles       = is_pkgFileNames is
    n              = length pkgFiles
    choiceIds      = [ "choice" ++ (show i) | i <- [0..(n-1)] ]
    pkgRefIds      = [ "pkg"    ++ (show i) | i <- [0..(n-1)] ]
    choicesOutline = mkChoicesOutline choiceIds
    choices        = map (\(x,y) -> mkChoice x x y) (choiceIds `zip` pkgRefIds)
    -- FIXME: installKBytes should not be "0"!
    pkgRefs        = map (\(x,(f,sz)) -> mkPkgRef x (show sz) f) (pkgRefIds `zip` pkgFiles)
