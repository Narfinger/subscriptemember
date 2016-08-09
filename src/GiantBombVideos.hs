{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}
module GiantBombVideos (updateVideos) where

import           Data.Aeson                    (FromJSON, decode, eitherDecode)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe
import qualified Data.List                         as L
import           Data.Time
import           Data.Text                     (Text)
import           Keys                 ( gbKey )
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( parseGiantBombTime )
import           YoutubeApiBase  ( Video(..)
                                 , Subscription(..)
                                 , VURL(..)
                                 , filterAndSortVideos
                                 )

data GiantBombResponse a = GiantBombResponse { results :: [a]
                                             , number_of_page_results :: Int
                                             , number_of_total_results :: Int
                                             , error :: Text
                                             } deriving (Show)

data GiantBombImage = GiantBombImage { medium_url :: Text
                                     , icon_url :: Text
                                     , screen_url :: Text
                                     , small_url :: Text
                                     , super_url :: Text
                                     , thumb_url :: Text
                                     , tiny_url :: Text
                                     } deriving (Show)


data GiantBombVideo = GiantBombVideo { name :: Text
                                     , image :: GiantBombImage
                                     , length_seconds :: Int
                                     , publish_date :: Text
                                     , site_detail_url :: Text
                                     , deck :: Text
                                     } deriving (Show)

$(deriveJSON defaultOptions ''GiantBombResponse)
$(deriveJSON defaultOptions ''GiantBombImage)
$(deriveJSON defaultOptions ''GiantBombVideo)


-- | get JSON
fetchJSON :: C.Request -> C.Manager -> IO (C.Response BL.ByteString)
fetchJSON = C.httpLbs

-- | extract video from response
extractVideo :: GiantBombVideo -> Video
extractVideo s =
  let gbs = Subscription { sid = "-1", channelname = "Giant Bomb", uploadPlaylist = "-1", thumbnail = "-1" } in
    Video {vidId = "", videotitle = name s, vidThumbnail = medium_url $ image s, publishedAt = parseGiantBombTime $ publish_date s, subscription = Just gbs
          , videoURL = GBURL (site_detail_url s), duration = length_seconds s}

-- | Transofmrs a single response to a maybe video using extractVideo
responseToVideo :: Maybe (GiantBombResponse GiantBombVideo) -> [Video]
responseToVideo Nothing  = []
responseToVideo (Just res) = map extractVideo (results res)

-- | query api to get the last 20 videos
getGiantBombResponse :: C.Manager -> IO (Maybe (GiantBombResponse GiantBombVideo))
getGiantBombResponse mgr = do
  req <- C.parseUrl $ "https://www.giantbomb.com/api/videos/?format=json&limit=1&api_key=" ++ gbKey
  decode <$> C.responseBody <$> fetchJSON req mgr
    -- debug function
    -- rt <- fetchJSON req mgr
    -- print rt
    -- print $ C.responseBody $ rt
    -- print $ (eitherDecode $ C.responseBody $ rt :: Either String (GiantBombResponse GiantBombVideo))
    -- return Nothing

-- | update videos
updateVideos :: C.Manager -> UTCTime -> IO [Video]
updateVideos mgr time =
  let fn = filterAndSortVideos time in
    fn <$> responseToVideo <$> getGiantBombResponse mgr
  -- let fn =  filterAndSortVids time . concat in
    -- fn <$> (fmap responseToVideo)
  

  -- debug function
  -- resp <- getGiantBombResponse mgr
  -- return [] 


  
  
