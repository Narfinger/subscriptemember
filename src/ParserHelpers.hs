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

-- data ParsedTime = ParsedTime { hours :: Int
--                              , minutes :: Int
--                              , seconds :: Int
--                              } deriving (Show)

-- parseNat :: Stream s m Char => ParsecT s u m Integer
-- parseNat = read <$> many1 digit

-- durationParser :: Stream s m Char => ParsecT s u m (Maybe ParsedTime)
-- durationParser = do
--   x <- string "PT"
--   h <- try parseNat char "H" 
--   -- m <- P.try parseNat P.char "M"
--   -- s <- P.try parseNat P.char "S"
--   --if h == 0 && m = 0 && s = 0 then return $ Nothing else
--   return $ Just (ParsedTime { hours = h, minutes = 0, seconds = 0})

-- | parse Duration format
-- | example times PT2H27M11S, PT10M12S


parseDuration :: T.Text -> Int
parseDuration t = 0
