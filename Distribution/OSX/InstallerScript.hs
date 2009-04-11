{-# LANGUAGE NoMonomorphismRestriction #-}

module Distribution.OSX.InstallerScript where

import Data.Maybe
import Distribution.PackageDescription.Configuration
import Text.XML.HXT.Arrow



simpleTag :: (ArrowXml a) => String -> String -> a n XmlTree
simpleTag tagName text = mkelem tagName [] [txt text]


mkTitle :: ArrowXml a => String -> a n XmlTree
mkTitle = simpleTag "title"


mkOptions :: (ArrowXml a) => a n XmlTree
mkOptions = mkelem "options" [ sattr "customize" "never"
                             , sattr "allow-external-scripts" "no"
                             , sattr "rootVolumeOnly" "false"] []


blurbAttrs :: (ArrowXml a) => [a n XmlTree]
blurbAttrs = [ sattr "language" "en"
             , sattr "mime-type" "text/plain" ]


blurb :: (ArrowXml a) => String -> String -> a n XmlTree
blurb tagName txt = mkelem tagName blurbAttrs [cdata txt]


mkReadme :: (ArrowXml a) => String -> a n XmlTree
mkReadme = blurb "readme"


mkWelcome :: (ArrowXml a) => String -> a n XmlTree
mkWelcome = blurb "welcome"


mkLicense :: (ArrowXml a) => String -> a n XmlTree
mkLicense = blurb "license"


mkConclusion :: (ArrowXml a) => String -> a n XmlTree
mkConclusion = blurb "conclusion"


cdata :: (ArrowXml cat) => String -> cat a XmlTree
cdata = (>>> mkCdata) . arr . const


mkLine :: (ArrowXml a) => String -> a n XmlTree
mkLine choiceId = mkelem "line" [sattr "choice" choiceId] []


mkChoicesOutline :: (ArrowXml a) => [String] -> a n XmlTree
mkChoicesOutline choiceIds =
    mkelem "choices-outline" [] (map mkLine choiceIds)


mkChoice :: (ArrowXml a) => String -> String -> String -> a n XmlTree
mkChoice id title pkgref =
    mkelem "choice"
           [ sattr "id"            id
           , sattr "title"         title
           , sattr "start_visible" "false" ]
           [ mkelem "pkg-ref" [sattr "id" pkgref] [] ]


mkPkgRef :: (ArrowXml a) => String -> String -> [Char] -> a n XmlTree
mkPkgRef id installKBytes pkgFileName =
    mkelem "pkg-ref"
           [ sattr "id"            id
           , sattr "installKBytes" installKBytes
           , sattr "version"       ""
           , sattr "auth"          "Root" ]
           [ txt $ "file:./Contents/Packages/" ++ pkgFileName ]


installerScriptHead :: (ArrowXml a) => [a n XmlTree] -> a n XmlTree
installerScriptHead body =
    root [] [ mkelem "installer-script"
                     [ sattr "minSpecVersion" "1.000000" ]
                     body ]


data InstallerScript = InstallerScript {
      title        :: String
    , background   :: Maybe String
    , welcome      :: Maybe String
    , readme       :: Maybe String
    , license      :: Maybe String
    , conclusion   :: Maybe String
    , pkgFileNames :: [String]
}


mkInstallerScript :: String           -- ^ title
                  -> Maybe String     -- ^ background image (FIXME: currently ignored)
                  -> Maybe String     -- ^ welcome blurb
                  -> Maybe String     -- ^ readme blurb
                  -> Maybe String     -- ^ license blurb
                  -> Maybe String     -- ^ conclusion blurb
                  -> [String]         -- ^ list of package files
                  -> InstallerScript
mkInstallerScript = InstallerScript


installerScript is =
    installerScriptHead $ concat [
                              [ mkTitle (title is) ]
                            , catMaybes [
                                  (welcome is)    >>= Just . mkWelcome
                                , (readme is)     >>= Just . mkReadme
                                , (license is)    >>= Just . mkLicense
                                , (conclusion is) >>= Just . mkConclusion ]
                            , [ choicesOutline ]
                            , choices
                            , pkgRefs ]
  where
    pkgFiles       = pkgFileNames is
    n              = length pkgFiles
    choiceIds      = [ "choice" ++ (show i) | i <- [0..(n-1)] ]
    pkgRefIds      = [ "pkg"    ++ (show i) | i <- [0..(n-1)] ]
    choicesOutline = mkChoicesOutline choiceIds
    choices        = map (\(x,y) -> mkChoice x x y) (choiceIds `zip` pkgRefIds)
    -- FIXME: installKBytes should not be "0"!
    pkgRefs        = map (\(x,y) -> mkPkgRef x "0" y) (pkgRefIds `zip` pkgFiles)


writeInstallerScript :: InstallerScript -> String -> IO [XmlTree]
writeInstallerScript is file = runX ( installerScript is
                                      >>> 
                                      writeDocument [(a_indent, v_1)] file )


installerScriptToString is = runX ( installerScript is
                                    >>>
                                    writeDocumentToString [(a_indent, v_1)] )
                             >>= return . concat
