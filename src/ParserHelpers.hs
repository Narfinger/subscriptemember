{-# LANGUAGE FlexibleContexts #-}
module ParserHelpers where

import Text.Parsec


data ParsedTime = ParsedTime { hours :: Int
                             , minutes :: Int
                             , seconds :: Int
                             } deriving (Show)

parseNat :: Stream s m Char => ParsecT s u m Integer
parseNat = read <$> many1 digit

durationParser :: Stream s m Char => ParsecT s u m (Maybe ParsedTime)
durationParser = do
  x <- string "PT"
  h <- try parseNat char "H" 
  -- m <- P.try parseNat P.char "M"
  -- s <- P.try parseNat P.char "S"
  --if h == 0 && m = 0 && s = 0 then return $ Nothing else
  return $ Just (ParsedTime { hours = h, minutes = 0, seconds = 0})

-- | parse Duration format
-- | example times PT2H27M11S, PT10M12S
