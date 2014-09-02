module Text.Log.Common.Parser
    (
--    Event parser
      readEvent
    , parseEvent
    , parseRemote
    , parseLogName
    , parseAuthUser
    , parseTimestamp
    , parseRequest
    , parseStatus
    , parseBytes
--    Parser utils
    , timestampFormat
    , parseOptional
    , parseOptionalQuoted
    , parseQuoted
    ) where

import Text.Log.Common.Types

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Time
import System.Locale
import qualified Data.ByteString.Char8 as B

-- Event parser

readEvent :: B.ByteString -> Maybe Event
readEvent src = case ee of { Right e -> Just e; _ -> Nothing }
    where ee = parseOnly parseEvent src

parseEvent :: Parser Event
parseEvent = Event
  <$> parseRemote
  <*> parseLogName
  <*> parseAuthUser
  <*> parseTimestamp
  <*> parseRequest
  <*> parseStatus
  <*> parseBytes

parseRemote :: Parser B.ByteString
parseRemote = takeTill (== ' ')

parseLogName :: Parser (Maybe B.ByteString)
parseLogName = char ' ' *> parseOptional ' '

parseAuthUser :: Parser (Maybe B.ByteString)
parseAuthUser = char ' ' *> parseOptional ' '

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  time <- string " [" *> takeTill (== ']') <* char ']'
  let time' = parseTime defaultTimeLocale timestampFormat (B.unpack time)
  maybe (fail "invalid timestamp") (return . zonedTimeToUTC) time'

parseRequest :: Parser B.ByteString
parseRequest = char ' ' *> parseQuoted

parseStatus :: Parser Int
parseStatus = char ' ' *> decimal

parseBytes :: Parser Int
parseBytes = char ' ' *> decimal

-- Parser utils

timestampFormat :: String
timestampFormat = "%d/%b/%Y:%H:%M:%S %z"

parseOptional :: Char -> Parser (Maybe B.ByteString)
parseOptional end = char '-' *> pure Nothing <|> Just <$> takeTill (== end)

parseOptionalQuoted :: Parser (Maybe B.ByteString)
parseOptionalQuoted = do
  opt <- char '"' *> parseOptional '"' <* char '"'
  return $ opt >>= \s -> case s of { "-" -> Nothing; _ -> opt }

parseQuoted :: Parser B.ByteString
parseQuoted = parseOptionalQuoted >>= return . maybe B.empty id
