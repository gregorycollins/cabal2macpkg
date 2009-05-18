module Distribution.OSX.PackageInfo
where

import Data.Maybe

import Text.XML.HXT.Arrow

------------------------------------------------------------------------
data PackageInfo = PackageInfo
    { pinfo_installKBytes :: Int
    , pinfo_numberOfFiles :: Int
    , pinfo_id            :: String
    , pinfo_preinstall    :: Maybe String
    , pinfo_postinstall   :: Maybe String }


emptyPackageInfo :: PackageInfo
emptyPackageInfo =  PackageInfo {
                      pinfo_installKBytes = 0
                    , pinfo_numberOfFiles = 0
                    , pinfo_id            = ""
                    , pinfo_preinstall    = Nothing
                    , pinfo_postinstall   = Nothing
                    }


mkPreinstall :: ArrowXml a => Maybe String -> [ a n XmlTree ]
mkPreinstall Nothing = []
mkPreinstall (Just s) = [ mkelem "preinstall"
                                 [ sattr "file" ("./" ++ s) ]
                                 [] ]

mkPostinstall :: ArrowXml a => Maybe String -> [ a n XmlTree ]
mkPostinstall Nothing = []
mkPostinstall (Just s) = [ mkelem "postinstall"
                                 [ sattr "file" ("./" ++ s) ]
                                 [] ]


mkPackageInfo :: ArrowXml a => PackageInfo -> a n XmlTree
mkPackageInfo (PackageInfo kb nf ident pre post) =
    root [] [ mkelem "pkg-info"
                     [ sattr "format-version" "2"
                     , sattr "identifier" ident
                     , sattr "version" "1"
                     , sattr "install-location" "/"
                     , sattr "auth" "root" ]
                     [ mkelem "payload"
                              [ sattr "installKBytes" (show kb)
                              , sattr "numberOfFiles" (show nf) ]
                              []
                     , mkelem "scripts" []
                              (concat [ mkPreinstall pre, mkPostinstall post ] ) ]
              ]



writePackageInfo :: String -> PackageInfo -> IO ()
writePackageInfo file pkg = runX (mkPackageInfo pkg
                                  >>>
                                  writeDocument [(a_indent, v_1)] file)
                            >> return ()

packageInfoToString :: PackageInfo -> IO String
packageInfoToString pkg = runX (mkPackageInfo pkg
                                >>>
                                writeDocumentToString [(a_indent, v_1)])
                          >>= return . concat

