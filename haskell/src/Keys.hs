{-# LANGUAGE OverloadedStrings #-}

module Keys where

import Data.Text
import           Network.OAuth.OAuth2

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"

-- If you look into my repository to get the keys, tough luck, I changed them
googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "270774910181-li3vetumviucp8itp6vq8af3pafok4vl.apps.googleusercontent.com"
                   , oauthClientSecret = "8BApGfHRqGwns3RU8TVY6YlE"
                   , oauthCallback = Just "http://127.0.0.1:9988/googleCallback"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
                   }

gbKey :: String
gbKey = "762469433941c623de5f13e44165c7bf8571aa2c"
