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

module AcidHandler where

import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (get, put)
import           Control.Monad.Trans
import           Data.Acid             (AcidState, Query, Update, makeAcidic)
import           Data.Acid.Advanced    (query', update')
import           Data.ByteString
import           Data.Data             (Data, Typeable)
import           Data.Maybe
import           Data.SafeCopy         (base, deriveSafeCopy)
import           Data.Time
import           Data.Time.Clock.POSIX
import           GoogleHandler
import           HelperFunctions       (deleteNth)
import qualified Network.HTTP.Conduit  as C
import           Network.OAuth.OAuth2
import           SubAndVideo           (Subscription (..), Video (..))


deriving instance Eq AccessToken
deriving instance Ord AccessToken
deriving instance Read AccessToken
deriving instance Data AccessToken
$(deriveSafeCopy 0 'base ''AccessToken)

-- | Complete State of the Server aka everything we save
data ServerState = ServerState { videos        :: [Video]
                               , subscriptions :: [Subscription]
                               , lastRefreshed :: UTCTime
                               , token         :: Maybe AccessToken
                               , rtoken        :: Maybe ByteString
                               } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''ServerState)

initialServerState :: ServerState
initialServerState = ServerState { videos = []
                                 , subscriptions = []
                                 , lastRefreshed =  posixSecondsToUTCTime 0
                                 , token = Nothing
                                 , rtoken = Nothing
                                 }


-- | Acid update of Subscriptions
updateSubs :: [Subscription] -> Update ServerState [Subscription]
updateSubs s = do
  c@ServerState{..} <- get
  put $ c { subscriptions = s}
  return s

-- | Acid query of Subscriptions
getSubs :: Query ServerState [Subscription]
getSubs = subscriptions <$> ask


-- | Acid query of Token
getAccessToken :: Query ServerState (Maybe AccessToken)
getAccessToken = token <$> ask

-- | Acid update of Token
writeAccessToken :: AccessToken -> Update ServerState AccessToken
writeAccessToken tk = do
  vs@ServerState{..} <- get
  put $ vs { token = Just tk }
  return tk

-- | Acid query of Refresh Token
getRefreshToken :: Query ServerState (Maybe ByteString)
getRefreshToken = rtoken <$> ask

-- | Acid update of Refresh Token
writeRefreshToken :: Maybe ByteString -> Update ServerState (Maybe ByteString)
writeRefreshToken tk = do
  vs@ServerState{..} <- get
  put $ vs { rtoken = tk }
  return tk

-- | Acid query of lastRefreshed
getLastRefreshed :: Query ServerState UTCTime
getLastRefreshed = lastRefreshed <$> ask

-- | Acid update of lastRefreshed
writeLastRefreshed :: UTCTime -> Update ServerState UTCTime
writeLastRefreshed t = do
  vs@ServerState{..} <- get
  put $ vs { lastRefreshed = t }
  return t

-- | Acid query videos
getVids :: Query ServerState [Video]
getVids = videos <$> ask

-- | Acid update vids
writeVids :: [Video] -> Update ServerState [Video]
writeVids v = do
  vs@ServerState{..} <- get
  put $ vs{videos = v}
  return v

deleteVid :: Int -> Update ServerState [Video]
deleteVid i = do
  vs@ServerState{..} <- get
  let v = deleteNth i videos
  put $ vs{videos = v }
  return v

deleteAll :: Update ServerState [Video]
deleteAll = do
  vs@ServerState{..} <- get
  put $ vs{videos = []}
  return []

$(makeAcidic ''ServerState ['getAccessToken, 'writeAccessToken, 'getRefreshToken, 'writeRefreshToken, 'updateSubs, 'getSubs, 'getLastRefreshed
                           ,'writeLastRefreshed,'getVids, 'writeVids, 'deleteVid, 'deleteAll])

-- | helper functions that asks a new token and saves it
saveNewToken :: C.Manager -> AcidState ServerState -> IO ()
saveNewToken mgr acid = do
  tk <- getToken mgr
  _ <- update' acid (WriteAccessToken tk)
  _ <- update' acid (WriteRefreshToken (refreshToken tk ))
  return ()

-- | If no token in Acid DB we get a new token
-- this is more complicated becuase i think we kill the refresh token or something if we just update it
newAccessTokenOrRefresh :: C.Manager -> AcidState ServerState -> IO ()
newAccessTokenOrRefresh mgr acid = do
  tk <- query' acid GetAccessToken
  case tk of
    Nothing -> saveNewToken mgr acid
    Just _ -> return ()
    -- Just x -> refreshAccessToken mgr acid
  return ()

-- | refreshes Token
refreshAccessToken :: C.Manager -> AcidState ServerState -> IO ()
refreshAccessToken mgr acid = do
  rtk <- fromJust <$> query' acid GetRefreshToken
  tk <- getNewAccessTokenFromRefreshToken rtk mgr
  _ <- update' acid (WriteAccessToken tk)
  return ()

-- | get token
acidGetAccessToken :: Control.Monad.Trans.MonadIO m => AcidState ServerState -> m (Maybe AccessToken)
acidGetAccessToken acid = query' acid GetAccessToken

acidGetRefreshToken :: Control.Monad.Trans.MonadIO m => AcidState ServerState -> m (Maybe ByteString)
acidGetRefreshToken acid = query' acid GetRefreshToken
