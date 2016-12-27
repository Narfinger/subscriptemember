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
module SubAndVideo (Subscription(..)
                    , VURL (..)
                    , Video (..)
                    , makeURLFromVURL
                    , makeURLFromVideo
                    , filterAndSortVideos) where

import           Data.Aeson    (FromJSON)
import           Data.Aeson.TH (constructorTagModifier, defaultOptions,
                                deriveJSON, fieldLabelModifier)
import           Data.Data     (Data, Typeable)
import qualified Data.List     as L
import           Data.Maybe
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Text     (Text, append, unpack)
import           Data.Time

-- | Main Datastructure for storing subscriptions
data Subscription = Subscription { sid            :: Text
                                 , channelname    :: Text
                                 , uploadPlaylist :: Text
                                 , thumbnail      :: Text
                                 } deriving (Eq, Ord, Read, Show, Data, Typeable)

data VURL = YTURL Text
          | GBURL Text
          deriving (Eq, Read, Show, Data, Typeable, Ord)

-- | Converts VURL to appropiate url
makeURLFromVURL :: VURL -> Text
makeURLFromVURL (YTURL a) = append "https://www.youtube.com/watch?v=" a
makeURLFromVURL (GBURL a) = a

-- | Main Datastructure for storing Youtube Videos
data Video = Video { vidId        :: Text
                   , videotitle   :: Text
                   , vidThumbnail :: Text
                   , publishedAt  :: UTCTime
                   , duration     :: Int
                   , subscription :: Maybe Subscription
                   , videoURL     :: VURL
                   } deriving (Eq, Read, Show, Data, Typeable)

instance Ord Video where
  x<= y = publishedAt x >= publishedAt y

-- | Filter Videos according to time
filterAndSortVideos :: UTCTime -> [Video] -> [Video]
filterAndSortVideos t xs = L.sort $ filter (\v -> publishedAt v > t) xs

-- | Video to url of the video
makeURLFromVideo :: Video -> Text
makeURLFromVideo v = makeURLFromVURL $ videoURL v

$(deriveSafeCopy 0 'base ''VURL)
$(deriveSafeCopy 0 'base ''Subscription)
$(deriveSafeCopy 0 'base ''Video)
