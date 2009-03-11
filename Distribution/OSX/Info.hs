module Distribution.OSX.Info (
    InfoPlist(..)
  , DescriptionPlist(..)
  , AuthorizationAction(..)
  , mkInfoPlist
  , mkDescriptionPlist
) where


import Data.Function
import Data.Maybe
import Data.Monoid

-- need this for XML character escaping
import Text.XML.Light.Output (showCData)
import Text.XML.Light.Types  (CData(..), CDataKind(..))


------------------------------------------------------------------------
data AuthorizationAction = NoAuthorization
                         | AdminAuthorization
                         | RootAuthorization
  deriving (Show)

-- note: not all fields are described here, only the ones I thought
-- you'd want to change (sorry, I'm lazy)

-- N.B. also we don't currently contain any fields that are valid for
-- metapackages
data InfoPlist = InfoPlist {
      plist_infoString          :: Maybe String
    , plist_identifier          :: Maybe String
    , plist_bundleName          :: Maybe String
    , plist_shortVersionString  :: Maybe String
    , plist_authorizationAction :: Maybe AuthorizationAction
    , plist_defaultLocation     :: Maybe String
    , plist_followLinks         :: Maybe Bool
    , plist_isRequired          :: Maybe Bool
    , plist_isRelocatable       :: Maybe Bool
    , plist_rootVolumeOnly      :: Maybe Bool
}


data DescriptionPlist = DescriptionPlist {
      dplist_title   :: Maybe String
    , dplist_version :: Maybe String
}


{-
instance Monoid InfoPlist where
    mempty = InfoPlist {
               plist_infoString          = Nothing
             , plist_identifier          = Nothing
             , plist_bundleName          = Nothing
             , plist_shortVersionString  = Nothing
             , plist_authorizationAction = Nothing
             , plist_defaultLocation     = Nothing
             , plist_followLinks         = Nothing
             , plist_isRequired          = Nothing
             , plist_isRelocatable       = Nothing
             , plist_rootVolumeOnly      = Nothing
             }


    a `mappend` b = InfoPlist {
                       plist_infoString          = o plist_infoString
                     , plist_identifier          = o plist_identifier
                     , plist_bundleName          = o plist_bundleName
                     , plist_shortVersionString  = o plist_shortVersionString
                     , plist_authorizationAction = o plist_authorizationAction
                     , plist_defaultLocation     = o plist_defaultLocation
                     , plist_followLinks         = o plist_followLinks
                     , plist_isRequired          = o plist_isRequired
                     , plist_isRelocatable       = o plist_isRelocatable
                     , plist_rootVolumeOnly      = o plist_rootVolumeOnly
                     }
      where
        -- monoid append using "Last" behaviour
        (*+*)    :: Maybe a -> Maybe a -> Maybe a
        (*+*)    = (getLast .) . (mappend `on` Last)

        o f = f a *+* f b
-}

emptyPlist :: InfoPlist
emptyPlist = InfoPlist {
               plist_infoString          = Nothing
             , plist_identifier          = Nothing
             , plist_bundleName          = Nothing
             , plist_shortVersionString  = Nothing
             , plist_authorizationAction = Nothing
             , plist_defaultLocation     = Nothing
             , plist_followLinks         = Nothing
             , plist_isRequired          = Nothing
             , plist_isRelocatable       = Nothing
             , plist_rootVolumeOnly      = Nothing
             }


emptyDescPlist = DescriptionPlist {
                   dplist_title   = Nothing
                 , dplist_version = Nothing
                 }


instance Show InfoPlist where
    show pkg = header ++ concat fields ++ footer
      where
        header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
                 \<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\"\n\
                 \  \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n\
                 \<plist version=\"1.0\">\n\
                 \<dict>\n"

        footer = "</dict>\n</plist>\n"

        fields = [ str  "CFBundleGetInfoString"        plist_infoString
                 , str  "CFBundleIdentifier"           plist_identifier
                 , str  "CFBundleName"                 plist_bundleName
                 , str  "CFBundleShortVersionString"   plist_shortVersionString
                 , str  "IFPkgFlagAuthorizationAction" $ (show `fmap`) . plist_authorizationAction
                 , str  "IFPkgFlagDefaultLocation"     plist_defaultLocation
                 , bool "IFPkgFlagFollowLinks"         plist_followLinks
                 , bool "IFPkgFlagIsRequired"          plist_isRequired
                 , bool "IFPkgFlagRelocatable"         plist_isRelocatable
                 , bool "IFPkgFlagRootVolumeOnly"      plist_rootVolumeOnly
                 ]

        key hdr = "\t<key>" ++ hdr ++ "</key>\n"

        str :: String -> (InfoPlist -> Maybe String) -> String
        str hdr f = maybe ""
                          (\v -> key hdr ++ "\t<string>" ++ esc v
                                 ++ "</string>\n")
                          (f pkg)

        bool :: String -> (InfoPlist -> Maybe Bool) -> String
        bool hdr f = maybe ""
                           (\v -> key hdr ++ "\t" ++ val v ++ "\n")
                           (f pkg)
          where
            val b = if b then "<true/>" else "<false/>"




instance Show DescriptionPlist where
    show pkg = header ++ concat fields ++ footer
      where
        header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
                 \<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\"\n\
                 \  \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n\
                 \<plist version=\"1.0\">\n\
                 \<dict>\n"

        footer = "</dict>\n</plist>\n"

        fields = [ str  "IFPkgDescriptionTitle"        dplist_title
                 , str  "IFPkgDescriptionVersion"      dplist_version ]

        key hdr = "\t<key>" ++ hdr ++ "</key>\n"

        str :: String -> (DescriptionPlist -> Maybe String) -> String
        str hdr f = maybe ""
                          (\v -> key hdr ++ "\t<string>" ++ esc v
                                 ++ "</string>\n")
                          (f pkg)

        bool :: String -> (DescriptionPlist -> Maybe Bool) -> String
        bool hdr f = maybe ""
                           (\v -> key hdr ++ "\t" ++ val v ++ "\n")
                           (f pkg)
          where
            val b = if b then "<true/>" else "<false/>"


------------------------------------------------------------------------
mkInfoPlist :: String           -- ^ package identifier
            -> String           -- ^ package version
            -> String           -- ^ package description
            -> String           -- ^ installation prefix
            -> InfoPlist
mkInfoPlist identifier version descr prefix =
    emptyPlist {
      plist_identifier         = Just ("org.haskell.libraries."
                                       ++ identifier)
    , plist_bundleName         = Just ("Haskell Library " ++ identifier)
    , plist_infoString         = Just (identifier ++ ": " ++ descr)
    , plist_shortVersionString = Just version
    , plist_defaultLocation    = Just prefix
    , plist_isRelocatable      = Just False
    , plist_rootVolumeOnly     = Just True
    }


------------------------------------------------------------------------
mkDescriptionPlist :: String -> String -> DescriptionPlist
mkDescriptionPlist title version =
    emptyDescPlist {
      dplist_title   = Just title
    , dplist_version = Just version
    }



esc :: String -> String
esc x = showCData (CData { cdVerbatim = CDataText
                         , cdData = x
                         , cdLine = Nothing })



