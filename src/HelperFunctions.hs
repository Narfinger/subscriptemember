module HelperFunctions ( firstLetterDown
                       , allToURLString
                       , thumbnailsLabelChange
                       , subscriptionLabelChange
                       , videoLabelChange
                       , deleteNth
                       , parseGoogleTime
                       , groupOn
                       ) where

import qualified Data.Char as Char    ( toLower )
import           Data.List as L
import           Data.Text as T
import qualified Data.Time as TI
import           Data.Time.Clock.POSIX

-- | takes a string and lowers the first character
firstLetterDown :: String -> String
firstLetterDown (h:t) = Char.toLower h : t
firstLetterDown [] = []

-- | sets all strings to "url"
allToURLString :: a -> String
allToURLString _ = "url"

-- | matches "def" to "default"
thumbnailsLabelChange :: String -> String
thumbnailsLabelChange "def" = "default"
thumbnailsLabelChange x = x

-- | matches "subscriptiontitle" to "title"
subscriptionLabelChange :: String -> String
subscriptionLabelChange "subscriptiontitle" = "title"
subscriptionLabelChange x = x

videoLabelChange :: String -> String
videoLabelChange "vidthumbnails" = "thumbnails"
videoLabelChange "vidresourceId" = "resourceId"
videoLabelChange "vidpublishedAt" = "publishedAt"
videoLabelChange "vidtitle" = "title"
videoLabelChange x = x


deleteNth :: Int -> [a] -> [a]
deleteNth i xs =
  let (ys,zs) = L.splitAt i xs in
  ys ++ (L.tail zs)

parseGoogleTime :: T.Text -> TI.UTCTime
parseGoogleTime t = posixSecondsToUTCTime 1

groupOn :: Int -> [a] -> [[a]]
groupOn _ [] = []
groupOn n l
  | n > 0 = (L.take n l) : (groupOn n (L.drop n l))
  | otherwise = error "Negative n"
