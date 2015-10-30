module HelperFunctions ( firstLetterDown ) where

import qualified Data.Char as Char    ( toLower )

firstLetterDown :: String -> String
firstLetterDown (head:tail) = Char.toLower head : tail
firstLetterDown [] = []


