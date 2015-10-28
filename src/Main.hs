{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module Main where

import           Control.Exception ( bracket )
import           Control.Monad        ( msum )
import           Control.Monad.Trans ( lift, liftIO )
import           Data.Acid  ( AcidState, makeAcidic, openLocalState )
import           Data.Acid.Advanced   ( query', update' )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Maybe ( fromJust )
import           Data.Time
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Happstack.Server     ( Response, ServerPart, ServerPartT, dir
                            , nullDir, nullConf, ok
                            , simpleHTTP, toResponse )
import           AcidHandler
import           GoogleHandler
import           YoutubeApi
import           Network.OAuth.OAuth2
import qualified Network.HTTP.Conduit as C

bodyTemplate :: H.Html ->H.Html
bodyTemplate body =
  H.html $ do
    H.head $ do
      H.title "Subscriptemember"
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

videoTemplate :: Video -> H.Html
videoTemplate v =
  H.tr $ do
    H.td $ do H.toHtml $ videotitle v
    H.td $ do "TMP URL"--url v

indexPage :: [Video] -> UTCTime -> H.Html
indexPage vs time =
  let t = formatTime defaultTimeLocale "Last Refreshed: %k:%M:%S %e.%m" time in
  bodyTemplate $ do
                    H.div ! A.class_ "col-md-8" $ do
                                   H.div ! A.class_ "row" $ do
                                     H.toHtml t
                                     H.table ! A.class_ "table table-striped" $ do
                                         mapM_ videoTemplate vs
                    H.div ! A.class_ "col-md-4" $ do
                                   H.div ! A.class_ "row" $ do
                                                    H.a ! A.href  "/subs" $ do "See Subscriptions"
                                   H.div ! A.class_ "row" $ do
                                                    H.a ! A.href  "/subsUp" $ do "Update and see Subscriptions"

                      
                      

subtotr :: Subscription -> H.Html
subtotr s =  H.tr $ do
  H.td $ do H.toHtml $ show $ sid s
  H.td $ do H.toHtml $ show $ channelname s

subPage :: [Subscription] -> H.Html
subPage s = bodyTemplate $
            H.table ! A.class_ "table table-striped" $ do
              H.tr $ do
                mapM_ subtotr s
subsHandler :: AcidState ServerState -> ServerPartT IO Response
subsHandler acid  = do
  subs <- query' acid GetSubs  
  ok $ toResponse $ subPage subs

subsAndUpdateHandler :: AcidState ServerState -> C.Manager -> AccessToken -> ServerPartT IO Response
subsAndUpdateHandler acid mgr tk = do
  s <- liftIO (updateSubscriptions mgr tk)
  subs <- update' acid (UpdateSubs s)
  ok $ toResponse $ subPage subs

indexHandler:: AcidState ServerState -> [Video] -> ServerPartT IO Response
indexHandler acid vs = do
  time <- query' acid GetLastRefreshed
  ok $ toResponse $ indexPage vs time

handlers :: AcidState ServerState -> C.Manager -> ServerPart Response
handlers acid mgr = do
--  vs <- acidGetVideos acid
  tk <- acidGetAccessToken acid
  let jtk = fromJust tk
  msum [ dir "subsUp" $ subsAndUpdateHandler acid mgr jtk
       , dir "subs" $ subsHandler acid
       , indexHandler acid []
       ]  
    
main :: IO ()
main = do
  mgr <- C.newManager C.conduitManagerSettings
  bracket (openLocalState initialServerState)
          (createCheckpointAndClose)
         (\acid -> do 
              newAccessTokenOrNothing acid;
                simpleHTTP nullConf (handlers acid mgr)            
         )
