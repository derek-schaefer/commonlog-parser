module Text.Log.Common.Types
    ( Event(..)
    ) where

import Data.Time

import qualified Data.ByteString.Char8 as B

data Event = Event
    { remote :: B.ByteString
    , logName :: Maybe B.ByteString
    , authUser :: Maybe B.ByteString
    , timestamp :: UTCTime
    , request :: B.ByteString
    , status :: Int
    , bytes :: Int
    } deriving (Show, Eq)
