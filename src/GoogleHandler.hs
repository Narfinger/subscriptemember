{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}

module GoogleHandler ( getToken
                     , getRefreshToken
                     ) where

import qualified Data.ByteString.Char8         as BS
import           Data.Maybe                    ( fromJust )
import qualified Network.HTTP.Conduit as C
import           Keys                          ( googleKey )
import           Network.OAuth.OAuth2


-- | General User Scope
-- googleScopeUserInfo :: QueryParams
-- googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- | Youtube Readonly Scope
googleScopeYoutube :: QueryParams
googleScopeYoutube = [("scope", "https://www.googleapis.com/auth/youtube.readonly")]

-- | Setup with google to get new token
getToken :: C.Manager -> IO AccessToken
getToken mgr = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` googleScopeYoutube
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken mgr googleKey code
    return token

getRefreshToken :: C.Manager -> AccessToken -> IO AccessToken
getRefreshToken mgr tk =
  let rtk = fromJust $ refreshToken tk in
  do
  (Right tk' ) <- fetchRefreshToken mgr googleKey rtk
  return tk'
