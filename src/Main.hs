{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module Main where

import           Control.Exception ( bracket )
import           Control.Monad        ( msum )
import           Data.Acid  ( AcidState, makeAcidic, openLocalState )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Maybe ( fromJust )
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Happstack.Server     ( Response, ServerPart, dir
                            , nullDir, nullConf, ok
                            , simpleHTTP, toResponse )
import           GoogleHandler
import           YoutubeApi
import           Network.OAuth.OAuth2


tmpdata :: [YoutubeVideo]
tmpdata = [ YoutubeVideo "test1" "http://test"
          , YoutubeVideo "test2" "http://tes2"
          , YoutubeVideo "test3" "http://tes3"
          ]
  
initialServerState :: ServerState
initialServerState = ServerState { videos = tmpdata
                                   , token = Nothing
                                   }

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
       vs <- acidGetVideos acid
       tk <- acidGetAccessToken acid
       let jtk = fromJust tk
       ok $ toResponse $ indexPage vs jtk
  ]  
    
main :: IO ()
main = do
  bracket (openLocalState initialServerState)
          (createCheckpointAndClose)
         (\acid -> do 
              newAccessTokenOrNothing acid;
              simpleHTTP nullConf (handlers acid))
