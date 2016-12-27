{-# LANGUAGE OverloadedStrings #-}

module Keys where

import Data.Text
import           Network.OAuth.OAuth2

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"

-- If you look into my repository to get the keys, tough luck, I changed them
googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "XXXXXX.apps.googleusercontent.com"
                   , oauthClientSecret = "XXX"
                   , oauthCallback = Just "http://127.0.0.1:9988/googleCallback"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
                   }

gbKey :: String
gbKey = "nothingisakey"
