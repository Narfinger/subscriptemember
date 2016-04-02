{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}

module YoutubeApiSubscriptions ( updateSubscriptions
                               ) where

import           Data.Maybe
import qualified Data.List                         as L
import           Data.Text                     (Text)
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( groupOn )
import YoutubeApiBase

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

-- | Given subscriptions, returns channel info as YoutubeResponse ContentDetails for all subscriptions in [Subscription] 
getUploadPlaylistForChannel :: C.Manager -> AccessToken -> [Subscription] -> IO [Maybe (YoutubeResponse ContentDetails)]
getUploadPlaylistForChannel mgr token channels =
  let channelids = (map . map) (textToByteString . sid) (groupOn 50 channels) in
  let urls = map (constructMultipleQuery "/channels?part=contentDetails&maxResults=50&id=") channelids in
  mapM (\xs -> fmap decode (authGetJSON mgr token xs :: IO (OAuth2Result (YoutubeResponse ContentDetails)))) urls

-- | Construct playlistid from response
constructPlaylistIds :: YoutubeItems ContentDetails -> Maybe Text
constructPlaylistIds x =
  let c = contentDetails x in
  case c of
  Nothing -> Nothing
  Just r -> Just (uploads $ relatedPlaylists r) 


-- | Given a list of subscriptions and a youtube response for this, update the subscription with the uploadPlaylist ids
extractPlaylist :: [Subscription] -> Maybe (YoutubeResponse ContentDetails) -> [Subscription]
extractPlaylist [] _ = []
extractPlaylist _ Nothing = []
extractPlaylist (x:xs) (Just cd) = 
  let velem = L.find (\y -> YoutubeApiBase.id y == sid x) (items cd) in
  case velem of
  Nothing -> []
  Just el -> 
    let i = constructPlaylistIds el  in
    x{uploadPlaylist = fromJust i} : extractPlaylist xs (Just cd)
