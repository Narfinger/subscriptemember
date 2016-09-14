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
import           Data.Text ( Text(..) )
import           Text.Blaze ((!))
import           Text.Blaze.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Spock
import           AcidHandler
import           HelperFunctions ( formatUTCToLocal, ourPrettyPrintTime, ourPrettyDurationTime )
import           YoutubeApiBase
import           YoutubeApiSubscriptions
import qualified YoutubeApiVideos as YTV 
import qualified GiantBombVideos as GBV
import           Network.OAuth.OAuth2
import qualified Network.HTTP.Conduit as C


data Sess = EmptySession



blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze = renderHtml
-- blaze = lazyBytes . renderHtml

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

--subsHandler :: AcidState ServerState -> ServerPartT IO Response
subsHandler acid  = do
  subs <- query' acid GetSubs  
  blaze $ subPage subs

-- subsAndUpdateHandler :: AcidState ServerState -> C.Manager -> AccessToken -> ServerPartT IO Response
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
  
-- upvidsHandler :: AcidState ServerState -> C.Manager -> AccessToken -> ServerPartT IO Response
upvidsHandler acid mgr tk = do
  let fn = upvids acid mgr tk
  lift (forkIO fn);
  redirect ("/"::Text)

-- deleteHandler :: AcidState ServerState -> Int -> ServerPartT IO Response
deleteHandler acid i = do
  update' acid (DeleteVid i)
  redirect ("/"::Text)

-- tokenRefreshHandler :: AcidState ServerState -> C.Manager -> ServerPartT IO Response
tokenRefreshHandler acid mgr = do
  lift (refreshAccessToken mgr acid);
  redirect ("/"::Text)

-- tokenHandler :: AccessToken -> B.ByteString -> ServerPartT IO Response
tokenHandler tk rtk = blaze $ tokenPage tk rtk

-- cleanAllHandler :: AcidState ServerState -> ServerPartT IO Response
cleanAllHandler acid = do
  update' acid DeleteAll
  redirect ("/"::Text)

-- indexHandler :: AcidState ServerState  -> ServerPartT IO Response
indexHandler acid = do
  time <- query' acid GetLastRefreshed
  formatedTime <- lift (formatUTCToLocal time)
  vs <- query' acid GetVids
  blaze $ indexPage vs formatedTime

-- handlers :: AcidState ServerState -> C.Manager -> ServerPart Response
handlers acid mgr = do
--  vs <- acidGetVideos acid
  tk <- acidGetAccessToken acid
  rtk <- acidGetRefreshToken acid
  let jtk = fromJust tk
  let jrtk = fromJust rtk
  get "subsUp"  $ subsAndUpdateHandler acid mgr jtk
  get "subs"    $ subsHandler acid
  get "upvids"  $ upvidsHandler acid mgr jtk
  get ("delete" <//> var) $ (\i -> deleteHandler acid i)
  get "cleanall"  $ cleanAllHandler acid
  get "tokenrefresh" $ tokenRefreshHandler acid mgr
  get "token" $ tokenHandler jtk jrtk
  get root $ indexHandler acid
  
main :: IO ()
main = do
  mgr <- C.newManager C.tlsManagerSettings
  bracket (openLocalState initialServerState)
          createCheckpointAndClose
         (\acid -> do 
              newAccessTokenOrRefresh mgr acid;
              print "Token found, doing refresh token";
              refreshAccessToken mgr acid;
              spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ()
              runSpock 8000 (spock spockCfg handlers)
         )
