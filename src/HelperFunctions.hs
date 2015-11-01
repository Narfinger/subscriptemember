module HelperFunctions ( firstLetterDown
                       , allToURLString
                       , thumbnailsLabelChange
                       , subscriptionLabelChange
                       , videoLabelChange
                       , deleteNth
                       ) where

import qualified Data.Char as Char    ( toLower )

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
  let (ys,zs) = splitAt i xs in
  ys ++ (tail zs)
