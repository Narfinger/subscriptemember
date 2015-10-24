{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}

module YoutubeApi where

--import GoogleHandler
import           Control.Applicative  ( (<$>) )
import           Control.Monad        ( msum, when )
import           Control.Monad.Reader ( ask )
import           Control.Monad.State  ( get, put )
import           Control.Monad.Trans
import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as BL
import           Data.Data            ( Data, Typeable )
import           Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Maybe
import           Data.Acid.Advanced   ( query', update' )
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Text                     (Text, unpack)
import qualified Network.HTTP.Conduit as C
import           Keys                          (googleKey)
import           Network.OAuth.OAuth2


baseurl :: B.ByteString
baseurl = "https://www.googleapis.com/youtube/v3"

data PageInfo = PageInfo { totalResults :: Int
                         , resultsPerPage :: Int
                         } deriving (Show)

data YoutubeResponse a = YoutubeResponse { kind :: Text
                                         , etag :: Text
                                         , nextPageToken :: Maybe Text
                                         , pageInfo :: PageInfo
                                         , items :: [YoutubeItems a]
                                         } deriving (Show)
                                                    
data YoutubeItems a = YoutubeItems { id :: Text
                                   , snippet :: Maybe a
                                   , contentDetails :: Maybe a
                                   } deriving (Show)

-- Youtube gives two channel ids, one which is mine and the correct one in resourceId
data YoutubeSubscription = YoutubeSubscription { publishedAt :: Text
                                               , title :: Text
                                               , description :: Text
                                               , resourceId :: YoutubeResource 
                                               } deriving (Show)

data YoutubeResource = YoutubeResource { -- kind :: Text
                                       channelId :: Text
                                       } deriving (Show)

data YoutubePlaylist = YoutubePlaylist { uploads :: Text
                                       } deriving (Show)

data YoutubeVideo = YoutubeVideo { publishedAt :: Text
                                 , title :: Text
                                 , description :: Text
                                 } deriving (Show)

$(deriveJSON defaultOptions ''PageInfo)
$(deriveJSON defaultOptions ''YoutubeResponse)
$(deriveJSON defaultOptions ''YoutubeItems)
$(deriveJSON defaultOptions ''YoutubeSubscription)
$(deriveJSON defaultOptions ''YoutubeResource)
$(deriveJSON defaultOptions ''YoutubePlaylist)

constructQuery :: B.ByteString -> B.ByteString
constructQuery = B.append baseurl

constructMultipleQuery :: B.ByteString -> [B.ByteString] -> B.ByteString
constructMultipleQuery b list = B.append b $ B.intercalate "," list

decode :: FromJSON a => Either BL.ByteString a -> Maybe a
decode (Left _) = Nothing
decode (Right x) = Just x

-- returns my subscriptions
getSubscriptionsForMe :: C.Manager -> AccessToken -> IO (Maybe (YoutubeResponse YoutubeSubscription))
getSubscriptionsForMe mgr token =
  let url = constructQuery "/subscriptions?&maxResults=50&part=snippet&mine=True" in
--  let flift = fmap (decodeResponse :: Either BL.ByteString BL.ByteString -> Maybe (YoutubeResponse YoutubeSubscription)) in
  fmap decode (authGetJSON mgr token url :: (IO (OAuth2Result (YoutubeResponse YoutubeSubscription))))
  

getUploadPlaylistForChannel :: C.Manager -> AccessToken -> [YoutubeSubscription] -> IO (Maybe (YoutubeResponse (YoutubePlaylist)))
getUploadPlaylistForChannel mgr token channels =
  let channelids = map (\x -> BC.pack $ unpack $ channelId x) channels in 
  let url = constructMultipleQuery "/channels?part=contentDetails&maxResults=50&fields=items%2FcontentDetailsid=" channelids in
  fmap decode (authGetJSON mgr token url :: IO (OAuth2Result (YoutubeResponse (YoutubePlaylist))))


getPlaylistItemsFromPlaylist :: C.Manager -> AccessToken -> YoutubePlaylist -> IO (Maybe (YoutubeResponse (YoutubeVideo)))
getPlaylistItemsFromPlaylist mgr token playlist =
   let url = "/playlistItems?part=snippet&playlistId=" ++ playlist in
   authGetJSON mgr token url

  -- update ids = getUploadPlaylistChannel(getSubscriptionsForMe) and save this
  -- check new videos for check all getPlaylistItemsFromPlaylist but this should be batchable
