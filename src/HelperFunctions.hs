module HelperFunctions ( firstLetterDown
                       , allToURLString
                       , thumbnailsLabelChange
                       , subscriptionLabelChange
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
