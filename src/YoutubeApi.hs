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
import qualified Data.ByteString.Lazy              as BL
import           Data.Data            ( Data, Typeable )
import           Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Maybe
import           Data.Acid.Advanced   ( query', update' )
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Text                     (Text)
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
                                                    
data YoutubeItems a = YoutubeItems { --kkind :: Text
                                   -- , eetag :: Text
                                   id :: Text
                                   , snippet :: a
                                   } deriving (Show)

data YoutubeSubscription = YoutubeSubscription { publishedAt :: Text
                                               , title :: Text
                                               , description :: Text
                                               , channelId :: Text
                                                              -- i ignored resourceId and thumbnails
                                               } deriving (Show)

$(deriveJSON defaultOptions ''PageInfo)
$(deriveJSON defaultOptions ''YoutubeResponse)
$(deriveJSON defaultOptions ''YoutubeItems)
$(deriveJSON defaultOptions ''YoutubeSubscription)

constructQuery :: B.ByteString -> B.ByteString
constructQuery = B.append baseurl


decode :: FromJSON a => Either BL.ByteString a -> Maybe a
decode (Left _) = Nothing
decode (Right x) = Just x

          -- returns my subscriptions
getSubscriptionsForMe :: FromJSON (YoutubeResponse YoutubeSubscription) =>
                         C.Manager -> AccessToken -> IO (Maybe (YoutubeResponse YoutubeSubscription))
getSubscriptionsForMe mgr token =
  let url = constructQuery "/subscriptions?&maxResults=50&part=snippet&mine=True" in
--  let flift = fmap (decodeResponse :: Either BL.ByteString BL.ByteString -> Maybe (YoutubeResponse YoutubeSubscription)) in
  fmap decode (authGetJSON mgr token url :: (IO (OAuth2Result (YoutubeResponse YoutubeSubscription))))
  

-- getUploadPlaylistForChannel :: FromJSON a=> C.Manager -> AccessToken -> [Channel] -> IO (OAuth2Result a)
-- getUploadPlaylistForChannel mgr token channel =
--     let url = baseurl ++ "channels?part=contentDetails&maxResults=50&fields=items%2FcontentDetailsid=" ++ channel in
--     authGetJSON mgr token url

-- getPlaylistItemsFromPlaylist :: FromJSON a => C.Manager -> AccessToken -> Playlist -> IO (OAuth2Result a)
-- getPlaylistItemsFromPlaylist mgr token playlist =
--   let url = "/playlistItems?part=snippet&playlistId=" ++ playlist in
--   authGetJSON mgr token url

  -- update ids = getUploadPlaylistChannel(getSubscriptionsForMe) and save this
  -- check new videos for check all getPlaylistItemsFromPlaylist but this should be batchable
