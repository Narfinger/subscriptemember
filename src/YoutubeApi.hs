{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}

module YoutubeApi ( updateSubscriptions
                  , updateVideos
                  , makeUrlFromId
                  , Video(..)      -- these are the general types we use for saving
                  , channelUrl
                  , Subscription(..) -- same for subscriptions
                  ) where

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
import Debug.Trace (trace)
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
  
-- | returns my subscriptions as a YoutubeResponse YoutubeSubscription
getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
getSubscriptionsForMe mgr token =
  let url = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
  authGetJSONPages mgr token url :: (IO [YoutubeItems YoutubeSubscription])
  --fmap decode (authGetJSON mgr token url :: (IO (OAuth2Result (YoutubeResponse YoutubeSubscription))))
  

-- | Given subscriptions, returns channel info as YoutubeResponse ContentDetails for all subscriptions in [Subscription] 
getUploadPlaylistForChannel :: C.Manager -> AccessToken -> [Subscription] -> IO [Maybe (YoutubeResponse ContentDetails)]
getUploadPlaylistForChannel mgr token channels =
  let channelids = (map . map) (textToByteString . sid) (groupOn 50 channels) in
  let urls = map (constructMultipleQuery "/channels?part=contentDetails&maxResults=50&id=") channelids in
  mapM (\xs -> fmap decode (authGetJSON mgr token xs :: IO (OAuth2Result (YoutubeResponse ContentDetails)))) urls

getPlaylistItemsFromPlaylist :: C.Manager -> AccessToken -> Subscription -> IO (Maybe (YoutubeResponse YoutubeVideo))
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

-- | Parses a list of Maybe Youtuberesponse YoutubeSubscription and returns the data parsed into a Subscription list 
extractSubscriptions :: [YoutubeItems YoutubeSubscription] -> [Subscription] 
extractSubscriptions = mapMaybe constructSubscriptionMaybe

-- | fetch subscriptions and returns them into a list
--updateSubscriptions :: C.Manager -> AccessToken -> IO [Subscription]
updateSubscriptions m tk = do
  subs <- extractSubscriptions <$> getSubscriptionsForMe m tk
  uploadsStuff <- getUploadPlaylistForChannel m tk subs
  let groupedSubs = groupOn 50 subs
  return $ L.concat $ L.zipWith extractPlaylist groupedSubs uploadsStuff

-- | Construct playlistid from response
constructPlaylistIds :: YoutubeItems ContentDetails -> Maybe Text
constructPlaylistIds x =
  let c = contentDetails x in
  case c of
  Nothing -> Nothing
  Just r -> Just (uploads $ relatedPlaylists r) 

-- | Given a list of subscriptions and a youtube response for this, update the subscription with the uploadPlaylist ids
extractPlaylist :: [Subscription] -> Maybe (YoutubeResponse ContentDetails) -> [Subscription]
extractPlaylist [] cd = []
extractPlaylist xs Nothing = []
extractPlaylist (x:xs) (Just cd) = 
  let elem = L.find (\y -> YoutubeApi.id y == sid x) (items cd) in
  case elem of
  Nothing -> []
  Just el -> 
    let i = constructPlaylistIds el  in
    x{uploadPlaylist = fromJust i} : extractPlaylist xs (Just cd)
    
-- | extract video from response
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

-- | Filter Videos according to time
filterAndSortVids :: UTCTime -> [Video] -> [Video]
filterAndSortVids t xs = L.sort $ filter (\v -> publishedAt v > t) xs

updateVideos :: C.Manager -> AccessToken -> UTCTime -> [Subscription] -> IO [Video]
updateVideos mgr tk time subs =
  let fn =  filterAndSortVids time . catMaybes in
  fn <$> mapM (fmap responseToVideo . getPlaylistItemsFromPlaylist mgr tk) subs

-- | get Video details for all video in list
getVideoDetails :: C.Manager -> AccessToken -> [Video] -> IO [Maybe (YoutubeResponse ContentDetails)]
getVideoDetails mgr token videos =
  let videoids = (map . map) (textToByteString . vidId) (groupOn 50 videos) in
  let urls = map (constructMultipleQuery "/videos?part=contentDetails&maxResults=50&id=") videoids in
  mapM (\xs -> fmap decode (authGetJSON mgr token xs :: IO (OAuth2Result (YoutubeResponse ContentDetails)))) urls


-- updateVideosWithTime :: C.Manager -> AccessToken -> [Video] -> [Video]
-- updateVideosWithTime m tk videos = do
--   extractVideoRuntime <$> getVideoDetails mgr tk videos
--   L.zipWith 
  
