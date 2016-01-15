{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}  
module Main where

import           Control.Concurrent ( forkIO )
import           Control.Exception ( bracket )
import           Control.Monad        ( msum )
import           Control.Monad.Trans
import           Data.Acid  ( AcidState, openLocalState )
import           Data.Acid.Advanced   ( query', update' )
import           Data.Acid.Local      ( createCheckpointAndClose )
import qualified Data.ByteString as B
import           Data.Maybe ( fromJust )
import           Data.Time
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Happstack.Server     ( Response, ServerPart, ServerPartT, dir
                                      , nullConf, ok, seeOther, path
                                      , simpleHTTP, toResponse )
import           AcidHandler
import           HelperFunctions
import           YoutubeApiBase
import           YoutubeApiSubscriptions
import           YoutubeApiVideos
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
        ! A.content "30"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "style.css"
    H.body $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "row" $ do
          H.h1 $ do
            H.a ! A.href "/" $ do "Youtube Subscriptemember"
          H.h3 "uploadids are not correctly given to channels"
          H.h3 "allow adding youtube urls"
          body

tokenPage :: AccessToken -> B.ByteString -> H.Html
tokenPage tk rtk =
  bodyTemplate $ do
    H.div ! A.class_ "row" $ do
      H.toHtml $ show tk
    H.div ! A.class_ "row" $ do
      H.toHtml $ show rtk

videoTemplate :: (Int, Video) -> H.Html
videoTemplate (i,v) =
  let deletelink =  "/delete/" ++ (show i) in
  H.tr $ do
    H.td $ do H.toHtml $ H.img ! A.src (H.preEscapedTextValue $ vidThumbnail v)
    H.td $ do H.toHtml $ videotitle v
    H.td $ do H.toHtml $ show $ ourPrettyPrintTime $ publishedAt v
    H.td $ do H.toHtml $ H.a ! A.href (H.preEscapedTextValue $ makeUrlFromId v) $ do "Play"
    H.td $ do H.toHtml $ H.a ! A.href (H.toValue deletelink) $ do "Delete"

indexPage :: [Video] -> UTCTime -> H.Html
indexPage videos time =
  let t = formatTime defaultTimeLocale "Last Refreshed: %k:%M:%S %e.%m" time
      vs = zip [0,1..] videos in
  bodyTemplate $ do
                    H.div ! A.class_ "col-md-8" $ do
                                   H.div ! A.class_ "row" $ do
                                     H.toHtml t
                                     H.table ! A.class_ "table table-striped" $ do
                                       H.tr $ do
                                         H.th "Thumbnail"
                                         H.th "Title"
                                         H.th "PlayButton"
                                       mapM_ videoTemplate vs
                    H.div ! A.class_ "col-md-4" $ do
                                   H.div ! A.class_ "row" $ do
                                     H.a ! A.href  "/subs" $ do "See Subscriptions"
                                   H.div ! A.class_ "row" $ do
                                     H.a ! A.href  "/subsUp" $ do "Update and see Subscriptions"
                                   H.div ! A.class_ "row" $ do
                                     H.a ! A.href  "/upvids" $ do "Update Videos"
                                   H.div ! A.class_ "row" $ do
                                     H.a ! A.href "/token" $ do "See Token"
                                   H.div ! A.class_ "row" $ do
                                     H.a ! A.href "/cleanall" $ do "Delete All Videos"
                                  

subtotr :: Subscription -> H.Html
subtotr s =  H.tr $ do
  H.td $ do H.img ! A.src (H.preEscapedTextValue $ thumbnail s) ! A.width "30" ! A.height "30"
  H.td $ do H.a ! A.href (H.toValue $ channelUrl s) $ do
              H.toHtml $ show $ channelname s
  H.td $ do H.toHtml $ show $ sid s
  H.td $ do H.toHtml $ show $ uploadPlaylist s

subPage :: [Subscription] -> H.Html
subPage s = bodyTemplate $
            H.div ! A.class_ "row" $ do
              H.p $ H.toHtml $ ("Number of Subscriptions: " ++  (show $ length s))
              H.div ! A.class_ "row" $ do
                H.table ! A.class_ "table table-striped" $ do
                  H.tr $ do
                    H.th "Icon"
                    H.th "Channel Name"
                    H.th "Channel ID"
                    H.th "Upload Playlist ID"
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
  seeOther ("/subs"::String) $ toResponse ()

upvids :: AcidState ServerState -> C.Manager -> AccessToken -> IO ()
upvids acid mgr tk = do
  subs <- query' acid GetSubs
  date <- query' acid GetLastRefreshed
  s <- liftIO (updateVideos mgr tk date subs)
  oldvids <- query' acid GetVids
  let nvids = s ++ oldvids
  nvids <- update' acid (WriteVids nvids)
  now <- getCurrentTime
  tmp <- update' acid (WriteLastRefreshed now)
  return ()
  
upvidsHandler :: AcidState ServerState -> C.Manager -> AccessToken -> ServerPartT IO Response
upvidsHandler acid mgr tk = do
  let fn = upvids acid mgr tk
  lift (forkIO fn);
  seeOther ("/"::String) $ toResponse ()

deleteHandler :: AcidState ServerState -> Int -> ServerPartT IO Response
deleteHandler acid i = do
  update' acid (DeleteVid i)
  seeOther ("/"::String) $ toResponse ()

tokenHandler :: AccessToken -> B.ByteString -> ServerPartT IO Response
tokenHandler tk rtk = do
  ok $ toResponse $ tokenPage tk rtk

cleanAllHandler :: AcidState ServerState -> ServerPartT IO Response
cleanAllHandler acid = do
  update' acid DeleteAll
  seeOther ("/"::String) $ toResponse ()

indexHandler:: AcidState ServerState  -> ServerPartT IO Response
indexHandler acid = do
  time <- query' acid GetLastRefreshed
  vs <- query' acid GetVids
  ok $ toResponse $ indexPage vs time

handlers :: AcidState ServerState -> C.Manager -> ServerPart Response
handlers acid mgr = do
--  vs <- acidGetVideos acid
  tk <- acidGetAccessToken acid
  rtk <- acidGetRefreshToken acid
  let jtk = fromJust tk
  let jrtk = fromJust rtk
  msum [ dir "subsUp" $ subsAndUpdateHandler acid mgr jtk
       , dir "subs" $ subsHandler acid
       , dir "upvids" $ upvidsHandler acid mgr jtk
       , dir "delete" $ path $ \i -> deleteHandler acid i
       , dir "cleanall" $ cleanAllHandler acid
       , dir "token" $ tokenHandler jtk jrtk
       , indexHandler acid
       ]
    
main :: IO ()
main = do
  mgr <- C.newManager C.tlsManagerSettings
  bracket (openLocalState initialServerState)
          createCheckpointAndClose
         (\acid -> do 
              newAccessTokenOrRefresh mgr acid;
              print "Token found, doing refresh token";
              refreshAccessToken mgr acid;
                simpleHTTP nullConf (handlers acid mgr)            
         )
