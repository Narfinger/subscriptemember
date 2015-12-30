{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}

module YoutubeApiBase (PageInfo
                      , YoutubeResponse
                      , YoutubeItems
                      , YoutubeThumbnails
                      , YoutubeURL
                      , YoutubeSubscription
                      , YoutubeResource
                      , ContentDetails
                      , RelatedPlaylists
                      , YoutubeVideo
                      , constructQueryString
                      , Subscription(..)
                      , Video(..))  where

import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as BL
import           Data.Data            ( Data, Typeable )
import           Data.Maybe
import qualified Data.List                         as L
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Time
import           Data.Text                     (Text, unpack, append)
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( firstLetterDown, thumbnailsLabelChange, subscriptionLabelChange, videoLabelChange
                                 , parseGoogleTime, groupOn )


-- | Base url for asking Youtube questions to google api
baseurl :: B.ByteString
baseurl = "https://www.googleapis.com/youtube/v3"

data PageInfo = PageInfo { totalResults :: Int
                         , resultsPerPage :: Int
                         } deriving (Show)

data YoutubeResponse a = YoutubeResponse { nextPageToken :: Maybe Text
                                         , pageInfo :: Maybe PageInfo
                                         , items :: [YoutubeItems a]
                                         } deriving (Show)
                                                    
data YoutubeItems a = YoutubeItems { id :: Text
                                   , snippet :: Maybe a
                                   , contentDetails :: Maybe a
                                   } deriving (Show)


data YoutubeThumbnails = YoutubeThumbnails { def :: YoutubeURL
                             } deriving (Show)

data YoutubeURL = YoutubeURL { url :: Text
                             } deriving (Show)


-- Youtube gives two channel ids, one which is mine and the correct one in resourceId
data YoutubeSubscription = YoutubeSubscription { -- publishedAt :: Text
                                               subscriptiontitle :: Text
                                               , description :: Text
                                               , resourceId :: YoutubeResource
                                               , thumbnails :: YoutubeThumbnails
                                               } deriving (Show)

data YoutubeResource = YoutubeResource { channelId :: Maybe Text
                                       , videoId :: Maybe Text
                                       } deriving (Show)

data ContentDetails = ContentDetails { relatedPlaylists  :: RelatedPlaylists
                                       } deriving (Show)

data RelatedPlaylists = RelatedPlaylists { uploads :: Text
                                         } deriving (Show)

data YoutubeVideo = YoutubeVideo { vidpublishedAt :: Text
                                 , vidtitle :: Text
                                 , vidthumbnails :: YoutubeThumbnails
                                 , vidresourceId :: YoutubeResource
                                 } deriving (Show)


$(deriveJSON defaultOptions ''PageInfo)
$(deriveJSON defaultOptions ''YoutubeResponse)
$(deriveJSON defaultOptions ''YoutubeItems)
$(deriveJSON defaultOptions{fieldLabelModifier = thumbnailsLabelChange} ''YoutubeThumbnails)
$(deriveJSON defaultOptions ''YoutubeURL)
$(deriveJSON defaultOptions{fieldLabelModifier = subscriptionLabelChange} ''YoutubeSubscription)
$(deriveJSON defaultOptions ''ContentDetails)
$(deriveJSON defaultOptions ''YoutubeResource)
$(deriveJSON defaultOptions{constructorTagModifier = firstLetterDown} ''RelatedPlaylists)
$(deriveJSON defaultOptions{fieldLabelModifier = videoLabelChange} ''YoutubeVideo)

-- | Text To Bytestring
textToByteString :: Text -> BC.ByteString
textToByteString = BC.pack . unpack

-- | constructs query using baseurl and the given bytestring
constructQuery :: BC.ByteString -> BC.ByteString
constructQuery = BC.append baseurl

constructQueryString :: String -> String
constructQueryString v = BC.unpack baseurl ++ v

-- | this constructs a query with comma separated inputs (comma is %2C in url code)
constructMultipleQuery :: BC.ByteString -> [BC.ByteString] -> BC.ByteString
constructMultipleQuery b list = B.append baseurl $ B.append b $ B.intercalate "%2C" list

-- | shorthand to decode a return from authGetJSON
decode :: FromJSON a => Either BL.ByteString a -> Maybe a
decode (Left l) =  Nothing
decode (Right x) = Just x


pagesAppend :: FromJSON (YoutubeResponse a) => YoutubeResponse a -> [YoutubeItems a] -> [YoutubeItems a]
pagesAppend x y = items x ++ y

getJSON :: FromJSON (YoutubeResponse a) => C.Manager -> AccessToken -> String -> IO (YoutubeResponse a)
getJSON mgr token url = do
  let burl = BC.pack url
  resp <- fmap decode (authGetJSON mgr token burl :: (FromJSON (YoutubeResponse a) => IO (OAuth2Result (YoutubeResponse a))))
  return $ fromJust resp

-- check out how i can do this lazyly
getJSONWithPages :: FromJSON (YoutubeResponse a) => C.Manager -> AccessToken -> String -> Maybe Text -> IO [YoutubeItems a]
getJSONWithPages _   _     _       Nothing = return []
getJSONWithPages mgr token url (Just pagetoken) = do
  let newurl = url ++ "&pageToken=" ++ unpack pagetoken ::String
  firstresp <- getJSON mgr token newurl
  sndresp <- getJSONWithPages mgr token (BC.unpack baseurl) (nextPageToken firstresp)
  return $ pagesAppend firstresp sndresp

authGetJSONPages :: FromJSON (YoutubeResponse a) => C.Manager -> AccessToken -> String -> IO [YoutubeItems a]
authGetJSONPages mgr token url = do
  resp <- getJSON mgr token url
  pagesAppend resp <$> getJSONWithPages mgr token url (nextPageToken resp)

makeUrlFromId :: Video -> Text
makeUrlFromId v = append "https://www.youtube.com/watch?v=" (vidId v)



-- | Main Datastructure for storing subscriptions
data Subscription = Subscription { sid :: Text
                                 , channelname :: Text
                                 , uploadPlaylist :: Text
                                 , thumbnail :: Text
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

channelUrl :: Subscription -> Text
channelUrl s = append ("https://www.youtube.com/channel/" ::Text)  (sid s) 

-- | Main Datastructure for storing videos
data Video = Video { vidId :: Text
                   , videotitle :: Text
                   , vidThumbnail :: Text
                   , publishedAt :: UTCTime
                   } deriving (Eq, Read, Show, Data, Typeable)

instance Ord Video where
  x<= y = publishedAt x <= publishedAt y

$(deriveSafeCopy 0 'base ''Subscription)
$(deriveSafeCopy 0 'base ''Video)

