{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Database
  ( Attendee(..), AttendeeDB(..)
  ) where

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Attoparsec as At
import qualified Data.ByteString as BS
import System.IO
import Control.Applicative
import qualified System.FilePath as F

data Attendee
  = Attendee
    { aId :: Int
    , aName :: String
    , aCircle :: String
    , aComment :: String
    }

instance A.ToJSON Attendee where
  toJSON (Attendee id name circle comment)
    = A.object
      [ "id" .= id
      , "name" .= name
      , "circle" .= circle
      , "comment" .= comment
      ]

instance A.FromJSON Attendee where
  parseJSON (A.Object o)
    = Attendee <$> o .: "id"
               <*> o .: "name"
               <*> o .: "circle"
               <*> o .: "comment"

data AttendeeDB
  = AttendeeDB
    { dbAttendees :: M.Map String Attendee
    , dbPath :: F.FilePath
    }

loadDB :: F.FilePath -> IO AttendeeDB
loadDB path = do
  m <- toMap `fmap` BS.readFile path
  return AttendeeDB {dbAttendees = m, dbPath = path}
  where
    toMap c = M.fromList $ zipWith (,) (map (show . aId) $ attendees c) $ attendees c

    attendees c = case attendees' c of
                    A.Success v -> v
                    _           -> undefined

    attendees' c = case At.parseOnly A.json c of
                    Right v -> AT.parse A.parseJSON v
                    Left e  -> A.Error e
