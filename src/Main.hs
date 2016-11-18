{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           AcidHandler
import           Control.Concurrent                   (forkIO)
import           Control.Exception                    (bracket)
import           Control.Monad                        (msum)
import           Control.Monad.Trans
import           Data.Acid                            (AcidState, createArchive,
                                                       openLocalState)
import           Data.Acid.Advanced                   (query', update')
import           Data.Acid.Local                      (createCheckpointAndClose)
import qualified Data.ByteString                      as B
import           Data.Maybe                           (fromJust)
import           Data.Text                            (Text (..))
import           Data.Time
import qualified GiantBombVideos                      as GBV
import           HelperFunctions                      (formatUTCToLocal,
                                                       ourPrettyDurationTime,
                                                       ourPrettyPrintTime)
import qualified Network.HTTP.Conduit                 as C
import           Network.OAuth.OAuth2
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Handler.WebSockets       (websocketsOr)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import qualified Network.WebSockets                   as WS
import           SubAndVideo                          (Subscription (..),
                                                       Video (..),
                                                       makeURLFromVideo)
import           System.Directory                     (getCurrentDirectory)
import           Text.Blaze                           ((!))
import           Text.Blaze.Html.Renderer.Utf8        (renderHtml)
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import           Web.Spock
import           Web.Spock.Config
import           YoutubeApiBase                       (channelUrl)
import           YoutubeApiSubscriptions
import qualified YoutubeApiVideos                     as YTV


type SiteAction ctx a = SpockActionCtx ctx () () () a

blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze = lazyBytes . renderHtml
{-# INLINE blaze #-}

bodyTemplate :: H.Html ->H.Html
bodyTemplate body =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Subscriptemember"
      H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js" $ ""
      H.script ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" $ ""
      H.meta ! A.httpEquiv "refresh"
        ! A.content "120"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "style.css"
    H.body $
      H.div ! A.class_ "container" $
        H.div ! A.class_ "row" $ do
          H.h1 $
            H.a ! A.href "/" $ "Youtube Subscriptemember"
          body
          H.script ! A.src "/subscriptemember.js" $ ""

tokenPage :: AccessToken -> B.ByteString -> H.Html
tokenPage tk rtk =
  bodyTemplate $ do
    H.div ! A.class_ "row" $
      H.toHtml $ show tk
    H.div ! A.class_ "row" $
      H.toHtml $ show rtk

videoTemplate :: (Int, Video) -> H.Html
videoTemplate (i,v) =
  let deletelink =  "/delete/" ++ show i in
  H.tr $ do
    H.td ! A.class_ "col-md-2" $ H.toHtml $ H.img ! A.src (H.preEscapedTextValue $ vidThumbnail v)  ! A.alt "Thumbnail" ! A.style "max-width:200px;"
    H.td ! A.class_ "col-md-2" $ H.toHtml $ maybe "" channelname $ subscription v
    H.td ! A.class_ "col-md-4" $ H.toHtml $ videotitle v
    H.td ! A.class_ "col-md-2" $ H.toHtml $ show $ ourPrettyPrintTime $ publishedAt v
    H.td ! A.class_ "col-md-1" $ H.toHtml $ ourPrettyDurationTime $ duration v
    H.td ! A.class_ "col-md-1" $ H.toHtml $ H.a ! A.href (H.preEscapedTextValue $ makeURLFromVideo v) $ "Play"
    H.td ! A.class_ "col-md-1" $ H.toHtml $ H.a ! A.href (H.toValue deletelink) $ "Delete"

indexPage :: [Video] -> String -> H.Html
indexPage videos time =
  let vs = zip [0,1..] videos
      l = "Number of Videos: " ++ show (length videos)
      totaltime = "Totaltime: " ++ ourPrettyDurationTime (foldl (\ x v -> x + duration v) 0 videos) in
    bodyTemplate $ do
                    H.div ! A.class_ "col-md-8" $ do
                                   H.div ! A.class_ "row" $ do
                                     H.div ! A.class_ "col-md-4" $
                                       H.toHtml time
                                     H.div ! A.class_ "col-md-4" $
                                       H.toHtml l
                                     H.div ! A.class_ "col-md-4" $
                                       H.toHtml totaltime
                                   H.div ! A.class_ "row" $
                                     H.table ! A.class_ "table table-striped" $ do
                                       H.tr $ do
                                         H.th "Thumbnail"
                                         H.th "Channel"
                                         H.th "Title"
                                         H.th "Published"
                                         H.th "Duration"
                                         H.th "Play"
                                         H.th "Delete"
                                       mapM_ videoTemplate vs
                    H.div ! A.class_ "col-md-2" $ do
                                   H.div ! A.class_ "row" $
                                     H.a ! A.href  "/subs" $ "See Subscriptions"
                                   H.div ! A.class_ "row" $
                                     H.a ! A.href  "/subsUp" $ "Update and see Subscriptions"
                                   H.div ! A.class_ "row" $
                                     H.a ! A.href  "/upvids" $ "Update Videos"
                                   H.div ! A.class_ "row" $
                                     H.a ! A.href "/token" $ "See Token"
                                   H.div ! A.class_ "row" $
                                     H.a ! A.href "/tokenrefresh" $ "Refresh Token"
                                   H.div ! A.class_ "row" $
                                     H.a ! A.href "/cleanall" $ "Delete All Videos"


subtotr :: Subscription -> H.Html
subtotr s =  H.tr $ do
  H.td $ H.img ! A.src (H.preEscapedTextValue $ thumbnail s) ! A.width "30" ! A.height "30"
  H.td $ H.a ! A.href (H.toValue $ channelUrl s) $
              H.toHtml $ show $ channelname s
  H.td $ H.toHtml $ show $ sid s
  H.td $ H.toHtml $ show $ uploadPlaylist s

subPage :: [Subscription] -> H.Html
subPage s = bodyTemplate $
            H.div ! A.class_ "row" $ do
              H.p $ H.toHtml ("Number of Subscriptions: " ++  show (length s))
              H.div ! A.class_ "row" $
                H.table ! A.class_ "table table-striped" $ do
                  H.tr $ do
                    H.th "Icon"
                    H.th "Channel Name"
                    H.th "Channel ID"
                    H.th "Upload Playlist ID"
                  H.tr $
                    mapM_ subtotr s

subsHandler :: MonadIO m => AcidState ServerState -> ActionCtxT ctx m b
subsHandler acid  = do
  subs <- query' acid GetSubs
  blaze $ subPage subs


subsAndUpdateHandler :: MonadIO m => AcidState ServerState -> C.Manager -> AccessToken -> ActionCtxT ctx m b
subsAndUpdateHandler acid mgr tk = do
  s <- liftIO (updateSubscriptions mgr tk)
  subs <- update' acid (UpdateSubs s)
  redirect ("/subs"::Text)

upvids :: AcidState ServerState -> C.Manager -> AccessToken -> IO ()
upvids acid mgr tk = do
  subs <- query' acid GetSubs
  date <- query' acid GetLastRefreshed
  s <- liftIO (YTV.updateVideos mgr tk date subs)
  s' <- liftIO (GBV.updateVideos mgr date)
  oldvids <- query' acid GetVids
  let nvids = s' ++ s ++ oldvids
  nvids <- update' acid (WriteVids nvids)
  now <- getCurrentTime
  tmp <- update' acid (WriteLastRefreshed now)
  return ()

upvidsHandler :: AcidState ServerState -> C.Manager -> AccessToken -> SiteAction ctx a
upvidsHandler acid mgr tk = do
  let fn = upvids acid mgr tk
  liftIO (forkIO fn);
  redirect ("/"::Text)

deleteHandler :: AcidState ServerState -> Int -> SiteAction ctx a
deleteHandler acid i = do
  update' acid (DeleteVid i)
  redirect ("/"::Text)

tokenRefreshHandler :: AcidState ServerState -> C.Manager -> SiteAction ctx a
tokenRefreshHandler acid mgr = do
  liftIO (refreshAccessToken mgr acid);
  redirect ("/"::Text)

tokenHandler :: MonadIO m => AccessToken -> B.ByteString -> ActionCtxT ctx m a
tokenHandler tk rtk = blaze $ tokenPage tk rtk

cleanAllHandler :: MonadIO m => AcidState ServerState -> ActionCtxT ctx m b
cleanAllHandler acid = do
  update' acid DeleteAll
  redirect ("/"::Text)

indexHandler :: AcidState ServerState  -> SiteAction ctx a
indexHandler acid = do
  time <- query' acid GetLastRefreshed
  formatedTime <- liftIO (formatUTCToLocal time)
  vs <- query' acid GetVids
  blaze $ indexPage vs formatedTime

handlers :: AcidState ServerState -> C.Manager -> AccessToken -> B.ByteString -> SpockM () () () ()
handlers acid mgr jtk jrtk = do
  get "subsUp"  $ subsAndUpdateHandler acid mgr jtk
  get "subs"    $ subsHandler acid
  get "upvids"  $ upvidsHandler acid mgr jtk
  get ("delete" <//> var) $ deleteHandler acid
  get "cleanall"  $ cleanAllHandler acid
  get "tokenrefresh" $ tokenRefreshHandler acid mgr
  get "token" $ tokenHandler jtk jrtk
  get root $ indexHandler acid

middlewares :: SpockM () () () ()
middlewares = do
  middleware logStdoutDev
  middleware $ unsafeStaticPolicy (addBase "./static/")

wsapplication :: AcidState ServerState  -> WS.ServerApp
wsapplication acid pending_conn = do
  conn <- WS.acceptRequest pending_conn
--  WS.forkPingThread conn 1
  WS.sendClose conn ("blubber" :: Text)
  --WS.sendTextData conn ("Hello, client!" :: Text)


main :: IO ()
main = do
  --getCurrentDirectory >>= print
  mgr <- C.newManager C.tlsManagerSettings
  bracket (openLocalState initialServerState)
          createCheckpointAndClose
         (\acid -> do
             createArchive acid
             newAccessTokenOrRefresh mgr acid;
             print "Token found, doing refresh token";
             refreshAccessToken mgr acid;
             spockCfg <- defaultSpockCfg () PCNoDatabase ()
             tk <- acidGetAccessToken acid
             rtk <- acidGetRefreshToken acid
             let jtk = fromJust tk      -- token
             let jrtk = fromJust rtk    -- refresh token
             spockApp <- spockAsApp $ (spock spockCfg (middlewares >> (handlers acid mgr jtk jrtk)))
             run 8000 $ websocketsOr WS.defaultConnectionOptions (wsapplication acid) spockApp
         )
