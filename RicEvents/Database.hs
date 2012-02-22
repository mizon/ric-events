{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Database
  ( Attendee(..), mkAttendee, AttendeeDB(..),
    put, get, all, loadDB, saveDB
  ) where

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Attoparsec as At
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import System.IO
import Control.Applicative
import qualified System.FilePath as F

import Prelude hiding (all)

data Attendee
  = Attendee
    { aId :: Maybe Int
    , aName :: String
    , aCircle :: String
    , aComment :: String
    }

mkAttendee :: String -> String -> String -> Attendee
mkAttendee name circle comment
  = Attendee Nothing name circle comment

instance A.ToJSON Attendee where
  toJSON (Attendee id name circle comment)
    = A.object
      [ "id" .=  id
      , "name" .= name
      , "circle" .= circle
      , "comment" .= comment
      ]

instance A.FromJSON Attendee where
  parseJSON (A.Object o)
    = Attendee <$> o .:? "id"
               <*> o .: "name"
               <*> o .: "circle"
               <*> o .: "comment"

data AttendeeDB
  = AttendeeDB
    { dbAttendees :: V.Vector Attendee
    , dbPath :: F.FilePath
    }

put :: Attendee -> AttendeeDB -> AttendeeDB
put a (db@AttendeeDB {dbAttendees = as})
  = db {dbAttendees = as `V.snoc` a {aId = Just $ V.length as}}

get :: Int -> AttendeeDB -> Maybe Attendee
get i (db@AttendeeDB {dbAttendees = as}) = as V.!? i

all :: AttendeeDB -> V.Vector Attendee
all = dbAttendees

loadDB :: F.FilePath -> IO AttendeeDB
loadDB path = do
  m <- attendees `fmap` BS.readFile path
  return AttendeeDB {dbAttendees = m, dbPath = path}
  where
    attendees bs
      = case attendees' bs of
         A.Success v -> v
         _           -> error "invalid json"

    attendees' bs
      = case At.parseOnly A.json bs of
          Right v -> AT.parse A.parseJSON v
          Left e  -> A.Error e

saveDB :: AttendeeDB -> IO ()
saveDB db = LBS.writeFile (dbPath db) $ A.encode $ all db
