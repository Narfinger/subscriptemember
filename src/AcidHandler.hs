{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module AcidHandler where

import           Control.Applicative  ( (<$>) )
import           Control.Monad        ( msum, when )
import           Control.Monad.Reader ( ask )
import           Control.Monad.State  ( get, put )
import           Control.Monad.Trans
import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Data            ( Data, Typeable )
import           Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Maybe
import           Data.Acid.Advanced   ( query', update' )
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Text                     (Text)
import qualified Network.HTTP.Conduit as C
import           Keys                          (googleKey)
import           Network.OAuth.OAuth2
import GoogleHandler
import YoutubeApi


deriving instance Eq AccessToken
deriving instance Ord AccessToken
deriving instance Read AccessToken
deriving instance Data AccessToken
$(deriveSafeCopy 0 'base ''AccessToken)

data ServerState = ServerState { videos :: [Video]
                               , subscriptions :: [Subscription]
                               , token :: Maybe AccessToken
                               } deriving (Eq, Ord, Read, Show, Data, Typeable)
                                         
$(deriveSafeCopy 0 'base ''ServerState)

initialServerState :: ServerState
initialServerState = ServerState { videos = []
                                 , subscriptions = []
                                 , token = Nothing
                                 }


-- Subs
updateSubs :: [Subscription] -> Update ServerState [Subscription]
updateSubs s = do
  c@ServerState{..} <- get
  put $ c { subscriptions = s}
  return s

getSubs :: Query ServerState [Subscription]
getSubs = subscriptions <$> ask


-- Token
getAccessToken :: Query ServerState (Maybe AccessToken)
getAccessToken = token <$> ask

writeAccessToken :: AccessToken -> Update ServerState AccessToken
writeAccessToken tk = do
  vs@ServerState{..} <- get
  put $ vs { token = Just tk }
  return tk


$(makeAcidic ''ServerState ['getAccessToken, 'writeAccessToken, 'updateSubs, 'getSubs])

  -- helper functions
saveNewToken :: AcidState ServerState -> IO ()
saveNewToken acid = do
  mgr <- C.newManager C.conduitManagerSettings
  token <- getToken mgr
  C.closeManager mgr
  update' acid (WriteAccessToken token)
  return ()
    
newAccessTokenOrNothing :: AcidState ServerState -> IO ()
newAccessTokenOrNothing acid = do
  tk <- query' acid GetAccessToken
  let newToken = isNothing tk
  when newToken (saveNewToken acid)
  return ()

acidGetAccessToken :: Control.Monad.Trans.MonadIO m => AcidState ServerState -> m (Maybe AccessToken)
acidGetAccessToken acid = query' acid GetAccessToken
