{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}
module GiantBombVideos (updateVideos) where

import qualified Data.ByteString.Char8             as BC
import           Data.Maybe
import qualified Data.List                         as L
import           Data.Time
import qualified Network.HTTP.Conduit as C
import           Network.OAuth.OAuth2
import           HelperFunctions ( groupOn )
import GiantBombVideos
