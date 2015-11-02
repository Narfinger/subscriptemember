{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}

module YoutubeApi ( updateSubscriptions
                  , updateVideos
                  , makeUrlFromId
                  , Video(..)      -- these are the general types we use for saving
                  , Subscription(..) -- same for subscriptions
                  ) where

import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as BL
import           Data.Data            ( Data, Typeable )
import           Data.Maybe
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Time
import           Data.Text                     (Text, unpack, append)
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import Debug.Trace (trace)
import           HelperFunctions ( firstLetterDown, thumbnailsLabelChange, subscriptionLabelChange, videoLabelChange
                                 , parseGoogleTime )

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
constructQuery :: B.ByteString -> B.ByteString
constructQuery = B.append baseurl


-- | this constructs a query with comma separated inputs (comma is %2C in url code)
constructMultipleQuery :: B.ByteString -> [B.ByteString] -> B.ByteString
constructMultipleQuery b list = B.append baseurl $ B.append b $ B.intercalate "%2C" list

-- | shorthand to decode a return from authGetJSON
decode :: FromJSON a => Either BL.ByteString a -> Maybe a
decode (Left l) =  Nothing
decode (Right x) = Just x

-- | returns my subscriptions as a YoutubeResponse YoutubeSubscription
getSubscriptionsForMe :: C.Manager -> AccessToken -> IO (Maybe (YoutubeResponse YoutubeSubscription))
getSubscriptionsForMe mgr token =
  let url = constructQuery "/subscriptions?&maxResults=50&part=snippet&mine=True" in
  fmap decode (authGetJSON mgr token url :: (IO (OAuth2Result (YoutubeResponse YoutubeSubscription))))
  

-- | Given subscriptions, returns channel info as YoutubeResponse ContentDetails for all subscriptions in [Subscription] 
getUploadPlaylistForChannel :: C.Manager -> AccessToken -> [Subscription] -> IO (Maybe (YoutubeResponse ContentDetails))
getUploadPlaylistForChannel mgr token channels =
  let channelids = map (textToByteString . sid) channels in 
  let url = constructMultipleQuery "/channels?part=contentDetails&maxResults=50&fields=items&id=" channelids in
  (fmap decode (authGetJSON mgr token url :: IO (OAuth2Result (YoutubeResponse ContentDetails))))


getPlaylistItemsFromPlaylist :: C.Manager -> AccessToken -> Subscription -> IO (Maybe (YoutubeResponse (YoutubeVideo)))
getPlaylistItemsFromPlaylist mgr token subscription =
   let askvalue = textToByteString $ uploadPlaylist subscription in
   let url = constructQuery (BC.append "/playlistItems?part=snippet&playlistId="  askvalue) in
   fmap decode (authGetJSON mgr token url :: IO (OAuth2Result (YoutubeResponse YoutubeVideo)))

-- | Main Datastructure for storing subscriptions
data Subscription = Subscription { sid :: Text
                                 , channelname :: Text
                                 , uploadPlaylist :: Text
                                 , thumbnail :: Text
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Main Datastructure for storing videos
data Video = Video { vidId :: Text
                   , videotitle :: Text
                   , vidThumbnail :: Text
                   , publishedAt :: UTCTime
                   } deriving (Eq, Ord, Read, Show, Data, Typeable)

makeUrlFromId :: Video -> Text
makeUrlFromId v = append "https://www.youtube.com/watch?v=" (vidId v)

$(deriveSafeCopy 0 'base ''Subscription)
$(deriveSafeCopy 0 'base ''Video)

-- | Takes YoutubeItem YoutubeSubscription and parses the data into a Subscription object if it exists
constructSubscriptionMaybe :: YoutubeItems YoutubeSubscription -> Maybe Subscription
constructSubscriptionMaybe x =
  let s = snippet x in
  case s of
  Nothing -> Nothing
  Just nx -> let r = channelId $ resourceId nx in
    let t = subscriptiontitle nx in
    let n = url $ def $ thumbnails nx in
    Just Subscription {sid = fromJust r, channelname = t, uploadPlaylist="", thumbnail = n}

-- | catMaybes lifted to work on a Maybe [a] and returns a [a] which can be empty
-- collapseMaybeList :: Maybe [Maybe a] -> [a]
-- collapseMaybeList Nothing = []
-- collapseMaybeList (Just x) = catMaybes x  

-- | Parses a list of Maybe Youtuberesponse YoutubeSubscription and returns the data parsed into a Subscription list 
extractSubscriptions :: Maybe (YoutubeResponse YoutubeSubscription) -> [Subscription] 
extractSubscriptions Nothing = []
extractSubscriptions (Just x) = mapMaybe constructSubscriptionMaybe (items x)

-- | fetch subscriptions and returns them into a list
updateSubscriptions :: C.Manager -> AccessToken -> IO [Subscription]
updateSubscriptions m tk = do
  subs <- (fmap extractSubscriptions) (getSubscriptionsForMe m tk)
  uploadsStuff <- getUploadPlaylistForChannel m tk subs
  return (extractPlaylist subs uploadsStuff)
   

constructPlaylistIds :: YoutubeItems ContentDetails -> Maybe Text
constructPlaylistIds x =
  let c = contentDetails x in
  case c of
  Nothing -> Nothing
  Just r -> Just (uploads $ relatedPlaylists r) 
  
extractPlaylist :: [Subscription] -> Maybe (YoutubeResponse ContentDetails) -> [Subscription]
extractPlaylist _ Nothing = []
extractPlaylist subs (Just x) =
  let ids = catMaybes $ map constructPlaylistIds (items x) in
  let construct = \id-> \s -> s{uploadPlaylist = id} in 
  zipWith construct ids subs


extractVideo :: YoutubeItems YoutubeVideo -> Maybe Video
extractVideo item =
  let s = snippet item in
  case s of
  Nothing -> Nothing
  Just snip -> let valuetitle = vidtitle snip in
    let valuethumb = url $ def $ vidthumbnails snip in
    let valuetitle = vidtitle snip
        valuepublishedat = parseGoogleTime $ vidpublishedAt snip in
    let valueid = videoId $ vidresourceId snip in
    Just Video { vidId = fromJust valueid, videotitle = valuetitle, vidThumbnail = valuethumb, publishedAt = valuepublishedat }


responseToVideo :: Maybe (YoutubeResponse YoutubeVideo) -> Maybe Video
responseToVideo Nothing = Nothing
responseToVideo (Just res) = extractVideo $ head $ items res
    
-- filterVidsForDate :: Data.Time -> [Video] -> [Video]
-- filterVidsForDate date = filter \v -> publishedAt 
                                      

filterAndSortVids :: UTCTime -> [Video] -> [Video]
filterAndSortVids t = filter (\v -> (publishedAt v)> t)

updateVideos :: C.Manager -> AccessToken -> UTCTime -> [Subscription] -> IO [Video]
updateVideos mgr tk time subs =
  let fn =  (filterAndSortVids time) . catMaybes in
  (fmap fn) $ sequence (map (\s -> (fmap responseToVideo) (getPlaylistItemsFromPlaylist mgr tk s))
                               subs)



