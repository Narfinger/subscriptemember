{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module AcidHandler where

import           Control.Monad        ( when )
import           Control.Monad.Reader ( ask )
import           Control.Monad.State  ( get, put )
import           Control.Monad.Trans
import           Data.Data            ( Data, Typeable )
import           Data.Acid            ( AcidState, Query, Update
                            , makeAcidic )
import           Data.Maybe
import           Data.Acid.Advanced   ( query', update' )
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( deleteNth )
import GoogleHandler
import YoutubeApi


deriving instance Eq AccessToken
deriving instance Ord AccessToken
deriving instance Read AccessToken
deriving instance Data AccessToken
$(deriveSafeCopy 0 'base ''AccessToken)

-- | Complete State of the Server aka everything we save
data ServerState = ServerState { videos :: [Video]
                               , subscriptions :: [Subscription]
                               , lastRefreshed :: UTCTime 
                               , token :: Maybe AccessToken
                               } deriving (Eq, Ord, Read, Show, Data, Typeable)
                                         
$(deriveSafeCopy 0 'base ''ServerState)

initialServerState :: ServerState
initialServerState = ServerState { videos = []
                                 , subscriptions = []
                                 , lastRefreshed =  posixSecondsToUTCTime 0
                                 , token = Nothing
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

$(makeAcidic ''ServerState ['getAccessToken, 'writeAccessToken, 'updateSubs, 'getSubs, 'getLastRefreshed, 'writeLastRefreshed,
                            'getVids, 'writeVids, 'deleteVid])

-- | helper functions that asks a new token and saves it 
saveNewToken :: AcidState ServerState -> IO ()
saveNewToken acid = do
  mgr <- C.newManager C.conduitManagerSettings
  token <- getToken mgr
  C.closeManager mgr
  update' acid (WriteAccessToken token)
  return ()

-- | If no token in Acid DB we get a new token


-- this is more complicated becuase i think we kill the refresh token or something if we just update it
newAccessTokenOrRefresh :: AcidState ServerState -> IO ()
newAccessTokenOrRefresh acid = do
  tk <- query' acid GetAccessToken
  case tk of
    Nothing -> (saveNewToken acid)
    Just x -> refreshAccessToken acid
  return ()

-- | refreshes token
refreshAccessToken :: AcidState ServerState -> IO ()
refreshAccessToken acid = do
  mgr <- C.newManager C.conduitManagerSettings
  otk <- (fmap fromJust) $ query' acid GetAccessToken
  tk <- getRefreshToken mgr otk
  update' acid (WriteAccessToken tk)
  print ("old access: " ++ (show $ expiresIn otk) ++ "  new in: " ++ (show $ expiresIn tk))
  print $ show tk
  return ()

-- | get token
acidGetAccessToken :: Control.Monad.Trans.MonadIO m => AcidState ServerState -> m (Maybe AccessToken)
acidGetAccessToken acid = query' acid GetAccessToken
