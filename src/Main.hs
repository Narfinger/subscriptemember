{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings #-}

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

data YoutubeVideo = YoutubeVideo { title :: String
                                 , url :: String
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

data ServerState = ServerState { videos :: [YoutubeVideo]
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
                                 }
appendVideos :: YoutubeVideo -> Update ServerState [YoutubeVideo]
appendVideos v = do
  vs@ServerState{..} <- get
  let nvs = v : videos
  put $ vs { videos =  nvs }
  return nvs
  
getVideos :: Query ServerState [YoutubeVideo]
getVideos = videos <$> ask

$(makeAcidic ''ServerState ['appendVideos, 'getVideos])


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

indexPage :: [YoutubeVideo] -> H.Html
indexPage vs = bodyTemplate $
               H.table ! A.class_ "table table-striped" $ do
                 mapM_ videoTemplate vs


handlers :: AcidState ServerState -> ServerPart Response
handlers acid = msum
  [
    do nullDir
       vs <- query' acid GetVideos
       ok $ toResponse $ indexPage vs
  ]



 
main :: IO ()
main =
  bracket (openLocalState initialServerState)
          (createCheckpointAndClose)
           (\acid ->
               simpleHTTP nullConf (handlers acid))
