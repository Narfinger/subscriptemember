{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}
module YoutubeApiVideos (updateVideos) where

import qualified Data.ByteString.Char8             as BC
import           Data.Maybe
import qualified Data.List                         as L
import           Data.Time
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( parseGoogleTime, groupOn )
import YoutubeApiBase

-- | JSON query to get playlist items from a subscriptions (not batched)
getPlaylistItemsFromPlaylist :: C.Manager -> AccessToken -> Subscription -> IO (Subscription, Maybe (YoutubeResponse YoutubeVideo))
getPlaylistItemsFromPlaylist mgr token sub =
   let askvalue = textToByteString $ uploadPlaylist sub in
   let purl = constructQuery (BC.append "/playlistItems?part=snippet&playlistId="  askvalue) in
   sequence (sub, fmap decode (authGetJSON mgr token purl :: IO (OAuth2Result (YoutubeResponse YoutubeVideo))))
    
-- | extract video from response
extractVideo :: YoutubeItems YoutubeVideo -> Maybe Video
extractVideo item =
  let s = snippet item in
  case s of
  Nothing -> Nothing
  Just snip ->
    let valuethumb = url $ def $ vidthumbnails snip
        valuetitle = vidtitle snip
        valuepublishedat = parseGoogleTime $ vidpublishedAt snip
        valueid = videoId $ vidresourceId snip in
    Just Video { vidId = fromJust valueid, videotitle = valuetitle, vidThumbnail = valuethumb, publishedAt = valuepublishedat
               , subscription = Nothing, videoURL = YTURL (fromMaybe "" valueid) }

-- | Transofmrs a single response to a maybe video using extractVideo
responseToVideo :: (Subscription, Maybe (YoutubeResponse YoutubeVideo)) -> [Video]
responseToVideo (_, Nothing) = []
responseToVideo (s, Just res) = map (\v -> v {subscription = Just s}) (mapMaybe extractVideo $ items res)
--responseToVideo (Just res) = extractVideo $ head $ items res

-- | Filter Videos according to time
filterAndSortVids :: UTCTime -> [Video] -> [Video]
filterAndSortVids t xs = L.sort $ filter (\v -> publishedAt v > t) xs

-- | Main function that gets called to get the current videos
updateVideos :: C.Manager -> AccessToken -> UTCTime -> [Subscription] -> IO [Video]
updateVideos mgr tk time subs =
  let fn =  filterAndSortVids time . concat in
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
  
