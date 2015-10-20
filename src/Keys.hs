{-# LANGUAGE OverloadedStrings #-}

module Keys where

import           Network.OAuth.OAuth2

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "xxxxxxxxxxxxxxx.apps.googleusercontent.com"
                   , oauthClientSecret = "xxxxxxxxxxxxxxxxxxxxxx"
                   , oauthCallback = Just "http://127.0.0.1:9988/googleCallback"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
                   }
