{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}

module YoutubeApi ( updateSubscriptions
                  , updateVideos
                  , Video(..)      -- these are the general types we use for saving
                  , Subscription(..) -- same for subscriptions
                  )
  where

--import GoogleHandler
import           Control.Applicative  ( (<$>) )
import           Control.Monad        ( msum, when )
import           Control.Monad.Reader ( ask )
import           Control.Monad.State  ( get, put )
import           Control.Monad.Trans
import           Data.Acid            ( AcidState, Query, Update
                                      , makeAcidic, openLocalState )
import           Data.Acid.Advanced   ( query', update' )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as BL
import           Data.Data            ( Data, Typeable )
import           Data.Maybe
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Text                     (Text, unpack)
import qualified Network.HTTP.Conduit as C
import           Keys                          (googleKey)
import           Network.OAuth.OAuth2
import Debug.Trace (trace)
import           HelperFunctions ( firstLetterDown )


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

-- Youtube gives two channel ids, one which is mine and the correct one in resourceId
data YoutubeSubscription = YoutubeSubscription { -- publishedAt :: Text
                                               subtitle :: Text
                                               , description :: Text
                                               , resourceId :: YoutubeResource 
                                               } deriving (Show)

data YoutubeResource = YoutubeResource { -- kind :: Text
                                       channelId :: Text
                                       } deriving (Show)

data ContentDetails = ContentDetails { relatedPlaylists  :: RelatedPlaylists
                                       } deriving (Show)

data RelatedPlaylists = RelatedPlaylists { uploads :: Text
                                         } deriving (Show)

data YoutubeVideo = YoutubeVideo { publishedAt :: Text
                                 , title :: Text
                                 -- , description :: Text
                                 } deriving (Show)


$(deriveJSON defaultOptions ''PageInfo)
$(deriveJSON defaultOptions ''YoutubeResponse)
$(deriveJSON defaultOptions ''YoutubeItems)
$(deriveJSON defaultOptions{fieldLabelModifier = \x -> if x == "subtitle" then "title" else x} ''YoutubeSubscription)
$(deriveJSON defaultOptions ''ContentDetails)
$(deriveJSON defaultOptions ''YoutubeResource)
$(deriveJSON defaultOptions{constructorTagModifier = firstLetterDown} ''RelatedPlaylists)
-- $(deriveJSON defaultOptions{constructorTagModifier = firstLetterDown}  ''RelatedPlaylists)
$(deriveJSON defaultOptions ''YoutubeVideo)

textToByteString :: Text -> BC.ByteString
textToByteString = BC.pack . unpack

constructQuery :: B.ByteString -> B.ByteString
constructQuery = B.append baseurl


--this constructs a query with comma separated inputs (comma is %2C in url code)
constructMultipleQuery :: B.ByteString -> [B.ByteString] -> B.ByteString
constructMultipleQuery b list = B.append baseurl $ B.append b $ B.intercalate "%2C" list

decode :: FromJSON a => Either BL.ByteString a -> Maybe a
decode (Left l) =  Nothing
decode (Right x) = Just x

-- returns my subscriptions
getSubscriptionsForMe :: C.Manager -> AccessToken -> IO (Maybe (YoutubeResponse YoutubeSubscription))
getSubscriptionsForMe mgr token =
  let url = constructQuery "/subscriptions?&maxResults=50&part=snippet&mine=True" in
--  let flift = fmap (decodeResponse :: Either BL.ByteString BL.ByteString -> Maybe (YoutubeResponse YoutubeSubscription)) in
  fmap decode (authGetJSON mgr token url :: (IO (OAuth2Result (YoutubeResponse YoutubeSubscription))))
  

getUploadPlaylistForChannel :: C.Manager -> AccessToken -> [Subscription] -> IO (Maybe (YoutubeResponse ContentDetails))
getUploadPlaylistForChannel mgr token channels =
  let channelids = map (\x -> textToByteString $ sid x) channels in 
  let url = constructMultipleQuery "/channels?part=contentDetails%2Csnippet&maxResults=50&fields=items&id=" channelids in
  (fmap decode (authGetJSON mgr token url :: IO (OAuth2Result (YoutubeResponse ContentDetails))))

--getIconsForChannel :: C.Manager -> AccessToken -> [Subscription] -> IO (Maybe (YoutubeResponse ))

-- getPlaylistItemsFromPlaylist :: C.Manager -> AccessToken -> YoutubePlaylist -> IO (Maybe (YoutubeResponse (YoutubeVideo)))
-- getPlaylistItemsFromPlaylist mgr token playlist =
--   let askvalue = textToByteString $ uploads playlist in
--   let url = constructQuery (BC.append "/playlistItems?part=snippet&playlistId="  askvalue) in
--   fmap decode (authGetJSON mgr token url :: IO (OAuth2Result (YoutubeResponse YoutubeVideo)))

data Subscription = Subscription { sid :: Text
                                 , channelname :: Text
                                 , uploadPlaylist :: Text
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Video = Video { vid :: Text
                   , videotitle :: Text
                   } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Subscription)
$(deriveSafeCopy 0 'base ''Video)

constructSubscriptionMaybe :: YoutubeItems YoutubeSubscription -> Maybe Subscription
constructSubscriptionMaybe x =
  let s = snippet x in
  case s of
  Nothing -> Nothing
  Just x -> let r = channelId $ resourceId x in
    let t = subtitle x in
    Just (Subscription {sid = r, channelname = t, uploadPlaylist=""})

collapseMaybeList :: Maybe [Maybe a] -> [a]
collapseMaybeList Nothing = []
collapseMaybeList (Just x) = catMaybes x  

extractSubscriptions :: Maybe (YoutubeResponse YoutubeSubscription) -> [Subscription] 
extractSubscriptions Nothing = []
extractSubscriptions (Just x) = catMaybes $ map constructSubscriptionMaybe (items x)

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
  let construct = \id -> \s -> s{uploadPlaylist = id} in 
  zipWith construct ids subs


updateVideos :: [Subscription] -> [Video]
updateVideos subs = []
