{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Database
  ( Attendee(..), AttendeeDB(..), put, get, all
  , loadDB, saveDB
  ) where

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Attoparsec as At
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO
import Control.Applicative
import qualified System.FilePath as F

import Prelude hiding (all)

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

put :: Attendee -> AttendeeDB -> AttendeeDB
put a (db@AttendeeDB {dbAttendees = as}) = db {dbAttendees = M.insert (aName a) a as}

get :: String -> AttendeeDB -> Maybe Attendee
get key (db@AttendeeDB {dbAttendees = as}) = M.lookup key as

all :: AttendeeDB -> [Attendee]
all = M.elems . dbAttendees

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

saveDB :: AttendeeDB -> IO ()
saveDB db = LBS.writeFile (dbPath db) $ A.encode $ all db
