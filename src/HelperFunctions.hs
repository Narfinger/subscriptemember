{-# LANGUAGE FlexibleContexts #-}
module HelperFunctions ( firstLetterDown
                       , allToURLString
                       , contentDetailChange
                       , combineWith
                       , itemLabelChange
                       , thumbnailsLabelChange
                       , subscriptionLabelChange
                       , videoLabelChange
                       , deleteNth
                       , ourPrettyPrintTime
                       , ourPrettyDurationTime
                       , groupOn
                       , textToByteString
                       ) where

import qualified Data.ByteString.Char8             as BC
import qualified Data.Char as Char    ( toLower )
import           Data.List as L
import           Data.Text as T       ( Text, unpack )
import qualified Data.Time as TI


-- | takes a string and lowers the first character
firstLetterDown :: String -> String
firstLetterDown (h:t) = Char.toLower h : t
firstLetterDown [] = []

-- | sets all strings to "url"
allToURLString :: a -> String
allToURLString _ = "url"

-- | mapping labelchange for contentdetail
contentDetailChange :: String -> String
contentDetailChange "durationDetails" = "duration"
contentDetailChange x = x

-- | maps items
itemLabelChange :: String -> String
itemLabelChange "iid" = "id"
itemLabelChange x = x

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

-- | prints integer to duration
ourPrettyDurationTime :: Int -> String
ourPrettyDurationTime secs 
    | h /= 0 = show h ++ ":" ++ (show m) ++ ":" ++ (show s)
    | m /= 0 =                   show m  ++ ":" ++ (show s)
    | otherwise =                                   show s
    where s = secs `mod` 60
          m = (secs `div` 60 ) `mod` 60
          h = ((secs `div` 60 ) `div` 60) `mod` 24
    
-- | Text To Bytestring
textToByteString :: T.Text -> BC.ByteString
textToByteString = BC.pack . T.unpack

-- | format the time for our thing
ourPrettyPrintTime :: TI.UTCTime -> String
ourPrettyPrintTime = TI.formatTime TI.defaultTimeLocale "%m-%d - %H:%M"

groupOn :: Int -> [a] -> [[a]]
groupOn _ [] = []
groupOn n l
  | n > 0 = L.take n l : groupOn n (L.drop n l)
  | otherwise = error "Negative n"


combineWith :: (a -> a -> Ordering) -> (b -> b -> Ordering) -> (a-> b -> c) -> [a] -> [b] -> [c]
combineWith ord1 ord2 map xr yr = zipWith map (sortBy ord1 xr) (sortBy ord2 yr)
