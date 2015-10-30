module HelperFunctions ( firstLetterDown
                       , allToURLString
                       , thumbnailsLabelChange
                       , subscriptionLabelChange
                       ) where

import qualified Data.Char as Char    ( toLower )

firstLetterDown :: String -> String
firstLetterDown (head:tail) = Char.toLower head : tail
firstLetterDown [] = []

allToURLString :: a -> String
allToURLString _ = "url"


thumbnailsLabelChange :: String -> String
thumbnailsLabelChange "def" = "default"
thumbnailsLabelChange x = x

subscriptionLabelChange :: String -> String
subscriptionLabelChange "subscriptiontitle" = "title"
subscriptionLabelChange x = x
