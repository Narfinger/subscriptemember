{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses,
    TypeFamilies, OverloadedStrings #-}

module GoogleHandler ( getToken
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
