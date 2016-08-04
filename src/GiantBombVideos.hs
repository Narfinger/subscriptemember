{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}
module GiantBombVideos (updateVideos) where

import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString.Char8             as BC
import           Data.Maybe
import qualified Data.List                         as L
import           Data.Time
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( groupOn )
import           YoutubeApiBase

data GiantBombResponse a = GiantBombResponse { -- nextPageToken :: Maybe Text
                                             -- , pageInfo :: Maybe PageInfo
                                             -- , items :: [YoutubeItems a]
                                             } deriving (Show)


data GiantBombVideo = GiantBombVideo { -- vidpublishedAt :: Text
                                     -- , vidtitle :: Text
                                     -- , vidthumbnails :: YoutubeThumbnails
                                     -- , vidresourceId :: YoutubeResource
                                     } deriving (Show)

$(deriveJSON defaultOptions ''GiantBombResponse)
$(deriveJSON defaultOptions ''GiantBombVideo)

-- | extract video from response
extractVideo :: GiantBombResponse GiantBombVideo -> Maybe Video
extractVideo item = Nothing
  -- let s = snippet item in
  --   Just Video { vidId = fromJust valueid, videotitle = valuetitle, vidThumbnail = valuethumb, publishedAt = valuepublishedat
  --              , subscription = Nothing, videoURL = YTURL (fromMaybe "" valueid) }

-- | Filter Videos according to time
filterAndSortVids :: UTCTime -> [Video] -> [Video]
filterAndSortVids t xs = L.sort $ filter (\v -> publishedAt v > t) xs


-- | Transofmrs a single response to a maybe video using extractVideo
responseToVideo :: Maybe (GiantBombResponse GiantBombVideo) -> [Video]
responseToVideo Nothing  = []
responseToVideo (Just res) = [] --map (\v -> v {subscription = Just s}) (mapMaybe extractVideo $ items res)

updateVideos :: C.Manager -> AccessToken -> UTCTime -> IO [Video]
updateVideos mgr tk time = return []
  -- let fn =  filterAndSortVids time . concat in
  -- fn <$> (fmap responseToVideo)
  
