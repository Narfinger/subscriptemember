{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Main where

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

initialServerState :: ServerState
initialServerState = ServerState { videos = []
                                 }
  
-- incCountBy :: Integer -> Update CounterState Integer
-- incCountBy n =
--     do c@CounterState{..} <- get
--        let newCount = count + n
--        put $ c { count = newCount }
--        return newCount

appendVideos :: YoutubeVideo -> Update ServerState [YoutubeVideo]
appendVideos v = do
  vs@ServerState{..} <- get
  let nvs = v : videos
  put $ vs { videos =  nvs }
  return nvs
  
getVideos :: Query ServerState [YoutubeVideo]
getVideos = videos <$> ask
                

-- getVideos :: Query VideosState VideosState
-- getVideos = ask

$(makeAcidic ''ServerState ['appendVideos, 'getVideos])

handlers :: AcidState ServerState -> ServerPart Response
handlers acid = msum
  [
    do nullDir
       vs <- query' acid GetVideos
       ok $ toResponse $ show vs
  ]

main :: IO ()
main =
  bracket (openLocalState initialServerState)
          (createCheckpointAndClose)
           (\acid ->
               simpleHTTP nullConf (handlers acid))
