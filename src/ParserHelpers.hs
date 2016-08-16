{-# LANGUAGE FlexibleContexts #-}
module ParserHelpers ( parseGoogleTime, parseGiantBombTime, parseDuration ) where

import           Data.Text as T       ( Text, unpack )
import qualified Data.Time as TI
import Text.Parsec


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

data ParsedTime = ParsedTime { hours :: Integer
                             , minutes :: Integer
                             , seconds :: Integer
                             } deriving (Show)

-- | Parser that parses natural numbers
parseNat :: Stream s m Char => ParsecT s u m Integer
parseNat = read <$> many1 digit

-- | Parser for duration format
-- | example times PT2H27M11S, PT10M12S
durationParser :: Stream s m Char => ParsecT s u m ParsedTime
durationParser = do
  x <- string "PT"
  h <- try $ parseNat ; lookAhead (string "H")
  m <- try $ parseNat ; lookAhead (string "M")
  s <- try $ parseNat ; lookAhead (string "S")
  return $ ParsedTime { hours = h, minutes = m, seconds = s}

-- | Helper function to make ParsedTime to Integer
parsedTimeToSecs :: ParsedTime -> Int
parsedTimeToSecs p = fromIntegral $ (hours p)  * 60 * 60 + (minutes p) * 60 + (seconds p)

-- | parse Duration format
parseDuration :: T.Text -> Int
parseDuration t =
  let errorfn = (\x -> -1) in
    either errorfn parsedTimeToSecs (parse durationParser "" t)
    
