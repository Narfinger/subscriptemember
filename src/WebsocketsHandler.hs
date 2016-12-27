{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module WebsocketsHandler (newWSState, writeConnection, updateWSClients, WSState) where

import           Control.Concurrent   (MVar, modifyMVar, modifyMVar_, newMVar,
                                       putMVar, readMVar)
import           Data.Text            (Text (..))
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import qualified Network.WebSockets   as WS


type WSState = MVar (Maybe WS.Connection)

newWSState :: IO (MVar (Maybe WS.Connection))
newWSState = newMVar Nothing

writeConnection :: WSState -> WS.Connection -> IO ()
writeConnection state conn = do
  putMVar state (Just conn)

updateWSClients :: WSState -> IO ()
updateWSClients wsstate = do
  state <- readMVar wsstate
  case state of
    Nothing -> return ()
    Just conn -> WS.sendTextData conn ("update" :: Text)
