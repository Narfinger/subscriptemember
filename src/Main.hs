{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module Main where

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Happstack.Server     ( Response, ServerPart, dir
                            , nullDir, nullConf, ok
                            , simpleHTTP, toResponse )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Network.URI
import Data.Text                     (Text)
import qualified Network.HTTP.Conduit as C
import Data.Aeson                    (FromJSON)
import Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Keys                          (googleKey)
import           Network.OAuth.OAuth2
import qualified Data.ByteString.Char8         as BS


data YoutubeVideo = YoutubeVideo { title :: String
                                 , url :: String
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)
deriving instance Eq AccessToken
deriving instance Ord AccessToken
deriving instance Read AccessToken
deriving instance Data AccessToken

googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]
-- data Token = Token { issued_to   :: Text
--                    , audience    :: Text
--                    , user_id     :: Maybe Text
--                    , scope       :: Text
--                    , expires_in  :: Integer
--                    -- , email          :: Maybe Text
--                    -- , verified_email :: Maybe Bool
--                    , access_type :: Text
--                    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''AccessToken)
-- $(deriveJSON defaultOptions ''AccessToken)

data ServerState = ServerState { videos :: [YoutubeVideo]
                               , token :: Maybe AccessToken
                               } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''YoutubeVideo)
$(deriveSafeCopy 0 'base ''ServerState)


tmpdata :: [YoutubeVideo]
tmpdata = [ YoutubeVideo "test1" "http://test"
          , YoutubeVideo "test2" "http://tes2"
          , YoutubeVideo "test3" "http://tes3"
          ]
  
initialServerState :: ServerState
initialServerState = ServerState { videos = tmpdata
                                   , token = Nothing
                                   }

getToken :: C.Manager -> IO AccessToken
getToken mgr = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` googleScopeUserInfo
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

getAccessToken :: Query ServerState (Just AccessToken)
getAccessToken = token <$> ask

writeAccessToken :: AccessToken -> Update ServerState AccessToken
writeAccessToken tk = do
  vs@ServerState{..} <- get
  put $ vs { token = Just tk }
  return tk

$(makeAcidic ''ServerState ['appendVideos, 'getVideos, 'getAccessToken, 'writeAccessToken])


bodyTemplate :: H.Html ->H.Html
bodyTemplate body =
  H.html $ do
    H.head $ do
      H.title "Amarok Control HASKELL"
      H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js" $ ""
      H.script ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" $ ""
      H.meta ! A.httpEquiv "refresh"
             ! A.content "60"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "style.css"
    H.body $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "row" $ do
          H.h1 $ "Youtube Subscriptemember"
          body

videoTemplate :: YoutubeVideo -> H.Html
videoTemplate v =
  H.tr $ do
    H.td $ do H.toHtml $ title v
    H.td $ do H.toHtml $ url v

indexPage :: [YoutubeVideo] -> AccessToken -> H.Html
indexPage vs tk = bodyTemplate $
                  H.table ! A.class_ "table table-striped" $ do
                    H.tr $ do
                      H.td $ do H.toHtml $ show tk
                    mapM_ videoTemplate vs


handlers :: AcidState ServerState -> ServerPart Response
handlers acid = msum
  [
    do nullDir
       vs <- query' acid GetVideos
       tk <- query' acid GetAccessToken
       ok $ toResponse $ indexPage vs tk
  ]
 
main :: IO ()
main = do
  -- this whole thing needs to be done only if no state was found
  mgr <- C.newManager C.conduitManagerSettings
  token <- getToken mgr
  C.closeManager mgr
  bracket (openLocalState (initialServerState token))
          (createCheckpointAndClose)
         (\acid -> do 
              tk <- query' acid GetAccessToken
              when (isNothing tk) do {
                mgr <- C.newManager C.conduitManagerSettings;
                token <- getToken mgr;
                C.closeManager mgr;
                update' acid (WriteAccessToken token)
                }
              simpleHTTP nullConf (handlers acid))
