{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
  TypeFamilies, RecordWildCards, OverloadedStrings,
  StandaloneDeriving #-}

module GoogleHandler where

import           Control.Applicative  ( (<$>) )
import           Control.Monad        ( msum, when )
import           Control.Monad.Reader ( ask )
import           Control.Monad.State  ( get, put )
import           Control.Monad.Trans
import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Data            ( Data, Typeable )
import           Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import           Data.Acid.Local      ( createCheckpointAndClose )
import           Data.Maybe
import           Data.Acid.Advanced   ( query', update' )
import           Data.SafeCopy        ( base, deriveSafeCopy )
import           Data.Text                     (Text)
import qualified Network.HTTP.Conduit as C
import           Keys                          (googleKey)
import           Network.OAuth.OAuth2

googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

googleScopeYoutube :: QueryParams
googleScopeYoutube = [("scope", "https://www.googleapis.com/auth/youtube.readonly")]

oauthScope :: QueryParams
oauthScope = [("scope", "https://www.googleapis.com/auth/youtube")]

getToken :: C.Manager -> IO AccessToken
getToken mgr = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` oauthScope
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken mgr googleKey code
    return token

