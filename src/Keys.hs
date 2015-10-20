{-# LANGUAGE OverloadedStrings #-}

module Keys where

import           Network.OAuth.OAuth2

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "270774910181-g7hj6igbi6tqskqjjk2vibheuoor6vut.apps.googleusercontent.com"
                   , oauthClientSecret = "34SzUVlFUeXxX1ia9-kOzAgy"
                   , oauthCallback = Just "http://127.0.0.1:9988/googleCallback"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
                   }
