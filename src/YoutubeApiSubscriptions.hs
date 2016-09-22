{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module YoutubeApiSubscriptions ( updateSubscriptions
                               ) where

import qualified Data.List            as L
import           Data.Maybe
import           Data.Text            (Text)
import           HelperFunctions      (groupOn)
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           YoutubeApiBase

-- | returns my subscriptions as a YoutubeResponse YoutubeSubscription
getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
getSubscriptionsForMe mgr token =
  let qurl = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
  authGetJSONPages mgr token qurl :: (IO [YoutubeItems YoutubeSubscription])
  --fmap decode (authGetJSON mgr token url :: (IO (OAuth2Result (YoutubeResponse YoutubeSubscription))))

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
updateSubscriptions :: C.Manager -> AccessToken -> IO [Subscription]
updateSubscriptions m tk = do
  subs <- extractSubscriptions <$> getSubscriptionsForMe m tk
  uploadsStuff <- getUploadPlaylistForChannel m tk subs
  let groupedSubs = groupOn 50 subs
  return $ L.concat $ L.zipWith extractPlaylist groupedSubs uploadsStuff

-- | Given subscriptions, returns channel info as YoutubeResponse YoutubeContentDetails for all subscriptions in [Subscription]
getUploadPlaylistForChannel :: C.Manager -> AccessToken -> [Subscription] -> IO [Maybe (YoutubeResponse YoutubeContentDetails)]
getUploadPlaylistForChannel mgr token channels =
  let channelids = (map . map) (textToByteString . sid) (groupOn 50 channels) in
  let urls = map (constructMultipleQuery "/channels?part=contentDetails&maxResults=50&id=") channelids in
  mapM (\xs -> fmap decode (authGetJSON mgr token xs :: IO (OAuth2Result (YoutubeResponse YoutubeContentDetails)))) urls

-- | Construct playlistid from response
constructPlaylistIds :: YoutubeItems YoutubeContentDetails -> Maybe Text
constructPlaylistIds x = let fn = \x -> return $ uploads x in
                           contentDetails x >>= relatedPlaylists >>= fn

-- | Given a list of subscriptions and a youtube response for this, update the subscription with the uploadPlaylist ids
extractPlaylist :: [Subscription] -> Maybe (YoutubeResponse YoutubeContentDetails) -> [Subscription]
extractPlaylist [] _ = []
extractPlaylist (x:xs) cdm =
  let eq = (\y -> YoutubeApiBase.iid y == sid x)
      velem = L.find eq $ concat $ (fmap items) $ cdm in
    let i = (fmap constructPlaylistIds) velem in
      x{uploadPlaylist = fromJust $ fromJust $ i} : extractPlaylist xs cdm
