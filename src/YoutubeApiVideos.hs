{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module YoutubeApiVideos (updateVideos) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.List             as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Text             as T
import           Data.Time
import           HelperFunctions       (combineWith, groupOn, textToByteString)
import qualified Network.HTTP.Conduit  as C
import           Network.OAuth.OAuth2
import           ParserHelpers         (parseDuration, parseGoogleTime)
import           YoutubeApiBase        (Subscription (..), VURL (..),
                                        Video (..), YoutubeContentDetails (..),
                                        YoutubeItems (..), YoutubeResource (..),
                                        YoutubeResponse (..),
                                        YoutubeThumbnails (..), YoutubeURL (..),
                                        YoutubeVideo (..),
                                        constructMultipleQuery, constructQuery,
                                        decode, filterAndSortVideos)

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
               , subscription = Nothing, videoURL = YTURL (fromMaybe "" valueid), duration = 0 }

-- | Transofmrs a single response to a maybe video using extractVideo
responseToVideo :: (Subscription, Maybe (YoutubeResponse YoutubeVideo)) -> [Video]
responseToVideo (_, Nothing) = []
responseToVideo (s, Just res) = map (\v -> v {subscription = Just s}) (mapMaybe extractVideo $ items res)
--responseToVideo (Just res) = extractVideo $ head $ items res

-- | Main function that gets called to get the current videos
updateVideos :: C.Manager -> AccessToken -> UTCTime -> [Subscription] -> IO [Video]
updateVideos mgr tk time subs =
  let fn =  filterAndSortVideos time . concat
      updtime = updateVideosWithTime mgr tk :: [Video] -> IO [Video] in
    L.sort <$> (updtime =<< fn <$> mapM (fmap responseToVideo . getPlaylistItemsFromPlaylist mgr tk) subs)



-- | get Video details for all video in list
getVideoDetails :: C.Manager -> AccessToken -> [Video] -> IO [Maybe (YoutubeResponse YoutubeContentDetails)]
getVideoDetails mgr token videos =
  let videoids = (map . map) (textToByteString . vidId) (groupOn 50 videos) in
  let urls = map (constructMultipleQuery "/videos?part=contentDetails&maxResults=50&id=") videoids in
  mapM (\xs -> fmap decode (authGetJSON mgr token xs :: IO (OAuth2Result (YoutubeResponse YoutubeContentDetails)))) urls

-- | gets the video details and extracts all the items
getDetailItems :: C.Manager -> AccessToken -> [Video] -> IO [YoutubeItems YoutubeContentDetails]
getDetailItems mgr tk videos = concat <$> map items <$> catMaybes <$> getVideoDetails mgr tk videos

getTime :: YoutubeItems YoutubeContentDetails -> T.Text
getTime i = fromMaybe "" (durationDetails =<< contentDetails i)

updateVideosWithTime :: C.Manager -> AccessToken -> [Video] -> IO [Video]
updateVideosWithTime mgr tk videos =
  let vsort = comparing vidId
      csort = comparing iid
      map = (\ x y -> x { duration = parseDuration $ getTime y}) :: Video -> YoutubeItems YoutubeContentDetails -> Video
      fn = combineWith vsort csort map videos :: [YoutubeItems YoutubeContentDetails] -> [Video] in
    fn <$> getDetailItems mgr tk videos

