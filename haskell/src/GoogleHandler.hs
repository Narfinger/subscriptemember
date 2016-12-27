{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module GoogleHandler ( getToken
                     , getNewAccessTokenFromRefreshToken
                     ) where

import qualified Data.ByteString.Char8 as BS
import           Keys                  (googleKey)
import qualified Network.HTTP.Conduit  as C
import           Network.OAuth.OAuth2


-- | General User Scope
-- googleScopeUserInfo :: QueryParams
-- googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- | Youtube Readonly Scope
googleScopeYoutube :: QueryParams
googleScopeYoutube = [("scope", "https://www.googleapis.com/auth/youtube.readonly")]

googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

-- | Setup with google to get new token
getToken :: C.Manager -> IO AccessToken
getToken mgr = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` (googleScopeYoutube++ googleAccessOffline)
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken mgr googleKey code
    return token


-- | Refresh the Token
getNewAccessTokenFromRefreshToken :: BS.ByteString -> C.Manager -> IO AccessToken
getNewAccessTokenFromRefreshToken rtk mgr = do
  let body = [("grant_type", "refresh_token")
             ,("refresh_token", rtk)]
             -- the idea is that we need to put a bunch of things into the post request that come from the oauth stuff
  (Right token) <- doJSONPostRequest mgr googleKey "https://www.googleapis.com/oauth2/v3/token" body
  print token
  return token
