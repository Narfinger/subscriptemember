{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module GiantBombVideos (updateVideos) where

import           Data.Aeson            (FromJSON, decode, eitherDecode)
import           Data.Aeson.TH         (constructorTagModifier, defaultOptions,
                                        deriveJSON, fieldLabelModifier)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.List             as L
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Time
import           Keys                  (gbKey)
import qualified Network.HTTP.Conduit  as C
import           Network.OAuth.OAuth2
import           ParserHelpers         (parseGiantBombTime)
import           SubAndVideo           (Subscription (..), VURL (..),
                                        Video (..), filterAndSortVideos)

limit :: String
limit = "10"

data GiantBombResponse a = GiantBombResponse { results                 :: [a]
                                             , number_of_page_results  :: Int
                                             , number_of_total_results :: Int
                                             , error                   :: T.Text
                                             } deriving (Show)

data GiantBombImage = GiantBombImage { medium_url :: T.Text
                                     , icon_url   :: T.Text
                                     , screen_url :: T.Text
                                     , small_url  :: T.Text
                                     , super_url  :: T.Text
                                     , thumb_url  :: T.Text
                                     , tiny_url   :: T.Text
                                     } deriving (Show)


data GiantBombVideo = GiantBombVideo { name            :: T.Text
                                     , image           :: GiantBombImage
                                     , length_seconds  :: Int
                                     , publish_date    :: T.Text
                                     , site_detail_url :: T.Text
                                     , deck            :: T.Text
                                     } deriving (Show)

$(deriveJSON defaultOptions ''GiantBombResponse)
$(deriveJSON defaultOptions ''GiantBombImage)
$(deriveJSON defaultOptions ''GiantBombVideo)


-- | get JSON
fetchJSON :: C.Request -> C.Manager -> IO (C.Response BL.ByteString)
fetchJSON req =
  let nreq = req { C.requestHeaders = [("User-Agent", "Subscriptemember")] } in
  C.httpLbs  nreq


-- | giantbomb at the moment returns https in json but does not serve images over https hence we fix this
fixthumb :: T.Text -> T.Text
fixthumb t = t --T.append "http" $ T.drop 5 t

-- | extract video from response
extractVideo :: GiantBombVideo -> Video
extractVideo s =
  let fixedthumbnail = fixthumb $ thumb_url $ image s
      gbs = Subscription { sid = "-1", channelname = "Giant Bomb", uploadPlaylist = "-1", thumbnail = "-1" } in
    Video {vidId = "", videotitle = name s, vidThumbnail = fixedthumbnail, publishedAt = parseGiantBombTime $ publish_date s, subscription = Just gbs
          , videoURL = GBURL (site_detail_url s), duration = length_seconds s}

-- | Transofmrs a single response to a maybe video using extractVideo
responseToVideo :: Maybe (GiantBombResponse GiantBombVideo) -> [Video]
responseToVideo res = concat $ fmap (map extractVideo) $ results <$> res

-- | query api to get the last 20 videos
getGiantBombResponse :: C.Manager -> IO (Maybe (GiantBombResponse GiantBombVideo))
getGiantBombResponse mgr = do
  req <- C.parseUrlThrow $ "https://www.giantbomb.com/api/videos/?format=json&limit=" ++ limit ++ "&api_key=" ++ gbKey
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




