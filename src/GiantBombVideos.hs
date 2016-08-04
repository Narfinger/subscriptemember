{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}
module GiantBombVideos (updateVideos) where

import           Data.Aeson                    (FromJSON, decode)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString.Char8             as BC
import           Data.Maybe
import qualified Data.List                         as L
import           Data.Time
import           Data.Text                     (Text)
import           Keys                 ( gbKey )
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( groupOn )
import           YoutubeApiBase  ( Video(..)
                                 , Subscription(..)
                                 , VURL(..)
                                 )

data GiantBombResponse a = GiantBombResponse { results :: [a]
                                             , number_of_page_results :: Int
                                             , number_of_results :: Int
                                             , error :: Text
                                             } deriving (Show)


data GiantBombVideo = GiantBombVideo { name :: Text
                                     , image :: Text
                                     , length_seconds :: Int
                                     , publish_date :: UTCTime
                                     , site_detail_url :: Text
                                     , deck :: Text
                                     } deriving (Show)

$(deriveJSON defaultOptions ''GiantBombResponse)
$(deriveJSON defaultOptions ''GiantBombVideo)


--fetchJSON ::  -> IO (C.Response BC.ByteString)
fetchJSON req mgr = C.httpLbs req mgr

-- | extract video from response
extractVideo :: GiantBombVideo -> Video
extractVideo s =
  let gbs = Subscription { sid = "-1", channelname = "Giant Bomb", uploadPlaylist = "-1", thumbnail = "-1" } in
    Video {vidId = "", videotitle = name s, vidThumbnail = image s, publishedAt = publish_date s, subscription = Just gbs
          , videoURL = GBURL (site_detail_url s)}

-- | Filter Videos according to time
filterAndSortVids :: UTCTime -> [Video] -> [Video]
filterAndSortVids t xs = L.sort $ filter (\v -> publishedAt v > t) xs

-- | Transofmrs a single response to a maybe video using extractVideo
responseToVideo :: Maybe (GiantBombResponse GiantBombVideo) -> [Video]
responseToVideo Nothing  = []
responseToVideo (Just res) = map extractVideo (results res)

-- | query api to get the last 20 videos
getGiantBombResponse :: C.Manager -> IO (Maybe (GiantBombResponse GiantBombVideo))
getGiantBombResponse mgr = do
  req <- C.parseUrl $ "https://www.giantbomb.com/api/videos/?format=json&limit=20&api_key=" ++ gbKey
  decode <$> C.responseBody <$> fetchJSON req mgr 


    --getJSON mgr token qurl :: (IO [GiantBombResponse GiantBombVideo])

updateVideos :: C.Manager -> UTCTime -> IO [Video]
updateVideos mgr time = fmap responseToVideo $ getGiantBombResponse mgr 
  -- let fn =  filterAndSortVids time . concat in
  -- fn <$> (fmap responseToVideo)
  
