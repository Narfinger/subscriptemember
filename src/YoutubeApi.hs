module YoutubeApi where

import GoogleHandler
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


baseurl = "https://www.googleapis.com/youtube/v3"

  -- returns my subscriptions
getSubscriptionsForMe :: FromJSON a=> C.Manager -> AccessToken -> Channel -> IO (OAuth2Result a)
getSubscriptionsForMe mgr token channel =
  let url = baseurl ++ "/subscriptions?&maxResults=50&part=snippet&mine=True" ++  channel in
  authGetJSON mgr token url

getUploadPlaylistForChannel :: FromJSON a=> C.Manager -> AccessToken -> [Channel] -> IO (OAuth2Result a)
getUploadPlaylistForChannel mgr token channel =
    let url = baseurl ++ "channels?part=contentDetails&maxResults=50&fields=items%2FcontentDetailsid=" ++ channel in
    authGetJSON mgr token url



getPlaylistItemsFromPlaylist :: FromJSON a => C.Manager -> AccessToken -> Playlist -> IO (OAuth2Result a)
getPlaylistItemsFromPlaylist mgr token playlist =
  let url = "/playlistItems?part=snippet&playlistId=" ++ playlist in
  authGetJSON mgr token url

  -- update ids = getUploadPlaylistChannel(getSubscriptionsForMe) and save this
  -- check new videos for check all getPlaylistItemsFromPlaylist but this should be batchable
