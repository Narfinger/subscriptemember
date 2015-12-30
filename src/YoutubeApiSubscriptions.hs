{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, StandaloneDeriving, OverloadedStrings #-}

module YoutubeApiSubscriptions ( updateSubscriptions
                               ) where

import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON, fieldLabelModifier, constructorTagModifier )
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as BL
import           Data.Data            ( Data, Typeable )
import           Data.Maybe
import qualified Data.List                         as L
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Time
import           Data.Text                     (Text, unpack, append)
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( firstLetterDown, thumbnailsLabelChange, subscriptionLabelChange, videoLabelChange
                                 , parseGoogleTime, groupOn )
import YoutubeApiBase

-- | returns my subscriptions as a YoutubeResponse YoutubeSubscription
getSubscriptionsForMe :: C.Manager -> AccessToken -> IO [YoutubeItems YoutubeSubscription]
getSubscriptionsForMe mgr token =
  let url = constructQueryString "/subscriptions?&maxResults=50&part=snippet&mine=True" in
  authGetJSONPages mgr token url :: (IO [YoutubeItems YoutubeSubscription])
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
--updateSubscriptions :: C.Manager -> AccessToken -> IO [Subscription]
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
extractPlaylist [] cd = []
extractPlaylist xs Nothing = []
extractPlaylist (x:xs) (Just cd) = 
  let elem = L.find (\y -> YoutubeApiBase.id y == sid x) (items cd) in
  case elem of
  Nothing -> []
  Just el -> 
    let i = constructPlaylistIds el  in
    x{uploadPlaylist = fromJust i} : extractPlaylist xs (Just cd)
