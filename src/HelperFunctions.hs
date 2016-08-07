module HelperFunctions ( firstLetterDown
                       , allToURLString
                       , thumbnailsLabelChange
                       , subscriptionLabelChange
                       , videoLabelChange
                       , deleteNth
                       , parseGoogleTime
                       , parseGiantBombTime
                       , ourPrettyPrintTime
                       , ourPrettyDurationTime
                       , groupOn
                       ) where

import qualified Data.Char as Char    ( toLower )
import           Data.List as L
import           Data.Text as T
import qualified Data.Time as TI

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

-- | mapping labelchanges for video
videoLabelChange :: String -> String
videoLabelChange "vidthumbnails" = "thumbnails"
videoLabelChange "vidresourceId" = "resourceId"
videoLabelChange "vidpublishedAt" = "publishedAt"
videoLabelChange "vidtitle" = "title"
videoLabelChange x = x


-- | removes the nth element of a list
deleteNth :: Int -> [a] -> [a]
deleteNth i xs =
  let (ys,zs) = L.splitAt i xs in
  ys ++ L.tail zs

-- | rfc3339 format string because I could not find it in the default library.
-- | This is an approximation of the real format because i don't quite understand it
-- | example date: 2015-11-08T20:10:43.000Z
rfc3339TimeFormat :: String
rfc3339TimeFormat = "%Y-%m-%dT%H:%M:%S.000Z"

-- | parse from text to UTCTime as the google format
parseGoogleTime :: T.Text -> TI.UTCTime
parseGoogleTime t = TI.parseTimeOrError True TI.defaultTimeLocale rfc3339TimeFormat (unpack t)

-- | giant bomb time format
-- | example date: 2016-08-03 14:47:00
giantBombTimeFormat :: String
giantBombTimeFormat = "%Y-%m-%d %H:%M:%S"

-- | parse GiantBomb time Format
parseGiantBombTime :: T.Text -> TI.UTCTime
parseGiantBombTime t = TI.parseTimeOrError True TI.defaultTimeLocale giantBombTimeFormat (unpack t)

-- | prints integer to duration
ourPrettyDurationTime :: Int -> String
ourPrettyDurationTime secs =
  let s = secs `mod` 60
      m = (secs `div` 60 ) `mod` 60
      h = ((secs `div` 60 ) `div` 60) `mod` 24 in
    let first = if h /= 0 then (show h) ++ ":" ++ (show m)
              else if m /= 0 then (show m) else "" in
    first ++ ":" ++ (show s)

-- | format the time for our thing
ourPrettyPrintTime :: TI.UTCTime -> String
ourPrettyPrintTime = TI.formatTime TI.defaultTimeLocale "%m-%d - %H:%M"

groupOn :: Int -> [a] -> [[a]]
groupOn _ [] = []
groupOn n l
  | n > 0 = L.take n l : (groupOn n (L.drop n l))
  | otherwise = error "Negative n"
