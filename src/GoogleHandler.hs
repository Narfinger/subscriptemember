{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module GoogleHandler where

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

googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

googleScopeYoutube :: QueryParams
googleScopeYoutube = [("scope", "https://www.googleapis.com/auth/youtube")]

oauthScope :: QueryParams
oauthScope = [("scope", "https://www.googleapis.com/auth/youtube")]

deriving instance Eq AccessToken
deriving instance Ord AccessToken
deriving instance Read AccessToken
deriving instance Data AccessToken

data YoutubeVideo = YoutubeVideo { title :: String
                                 , url :: String
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

data ServerState = ServerState { videos :: [YoutubeVideo]
                               , token :: Maybe AccessToken
                               } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''AccessToken)
$(deriveSafeCopy 0 'base ''ServerState)
$(deriveSafeCopy 0 'base ''YoutubeVideo)

getToken :: C.Manager -> IO AccessToken
getToken mgr = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` oauthScope
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken mgr googleKey code
    return token

appendVideos :: YoutubeVideo -> Update ServerState [YoutubeVideo]
appendVideos v = do
  vs@ServerState{..} <- get
  let nvs = v : videos
  put $ vs { videos =  nvs }
  return nvs
  
getVideos :: Query ServerState [YoutubeVideo]
getVideos = videos <$> ask

getAccessToken :: Query ServerState (Maybe AccessToken)
getAccessToken = token <$> ask

writeAccessToken :: AccessToken -> Update ServerState AccessToken
writeAccessToken tk = do
  vs@ServerState{..} <- get
  put $ vs { token = Just tk }
  return tk

$(makeAcidic ''ServerState ['appendVideos, 'getVideos, 'getAccessToken, 'writeAccessToken])


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


acidGetVideos :: Control.Monad.Trans.MonadIO m => AcidState ServerState -> m [YoutubeVideo]
acidGetVideos acid = query' acid GetVideos


acidGetAccessToken :: Control.Monad.Trans.MonadIO m => AcidState ServerState -> m (Maybe AccessToken)
acidGetAccessToken acid = query' acid GetAccessToken
