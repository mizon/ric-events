{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Database
  ( Attendee(..), mkAttendee, AttendeeDB(..)
  , DBAction, withDB, getAttendee, putAttendee, deleteAttendee, getAllAttendees
  , get, put, delete, all, loadDB, saveDB
  ) where

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec as At
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified Data.Maybe as Mb
import qualified Data.Digest.Pure.SHA as SHA
import System.IO
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as S
import qualified Control.Monad.Error as E
import qualified System.FilePath as F

import Prelude hiding (all)

data Attendee = Attendee
  { aId :: Maybe Int
  , aName :: String
  , aCircle :: String
  , aComment :: String
  , aEncryptedPassword :: String
  }
  deriving Show

mkAttendee :: String -> String -> String -> LBS.ByteString -> Attendee
mkAttendee name circle comment password
  = Attendee Nothing name circle comment $ encrypt password

instance A.ToJSON Attendee where
  toJSON (Attendee id_ name circle comment password) = A.object
    [ "id" .=  id_
    , "name" .= name
    , "circle" .= circle
    , "comment" .= comment
    , "encrypted-password" .= password
    ]

instance A.FromJSON Attendee where
  parseJSON (A.Object o)
    = Attendee <$> o .:? "id"
               <*> o .: "name"
               <*> o .: "circle"
               <*> o .: "comment"
               <*> o .: "encrypted-password"

data AttendeeDB = AttendeeDB
  { dbAttendees :: V.Vector (Maybe Attendee)
  , dbPath :: F.FilePath
  }
  deriving Show

type DBAction = S.StateT AttendeeDB (Either String)

withDB :: F.FilePath -> DBAction a -> IO (Either String a)
withDB path action = do
  e <- S.runStateT action <$> loadDB path
  case e of
    Right (a, db) -> do
      saveDB db
      return $ Right a
    Left err -> return $ Left err

getAttendee :: Int -> DBAction (Maybe Attendee)
getAttendee = S.gets . get

putAttendee :: Attendee -> DBAction ()
putAttendee = S.modify . put

deleteAttendee :: LBS.ByteString -> Int -> DBAction ()
deleteAttendee pass id_ = do
  status <- delete pass id_ <$> S.get
  case status of
    Right db -> S.put db
    Left err -> E.throwError err

getAllAttendees :: DBAction (V.Vector Attendee)
getAllAttendees = S.gets all

get :: Int -> AttendeeDB -> Maybe Attendee
get i AttendeeDB {dbAttendees = as} = join $ as V.!? i

put :: Attendee -> AttendeeDB -> AttendeeDB
put a db@AttendeeDB {dbAttendees = as}
  = db {dbAttendees = as `V.snoc` Just a {aId = Just $ V.length as}}

delete :: LBS.ByteString -> Int -> AttendeeDB -> Either String AttendeeDB
delete pass i db@AttendeeDB {dbAttendees = as} = deleteIfExist $ get i db
  where
    deleteIfExist (Just Attendee {aEncryptedPassword = ep})
      | ep == encrypt pass = Right db {dbAttendees = as V.// [(i, Nothing)]}
      | otherwise          = Left "invalid password"
    deleteIfExist Nothing  = Left "invalid id"

all :: AttendeeDB -> V.Vector Attendee
all = V.map Mb.fromJust . V.filter Mb.isJust . dbAttendees

loadDB :: F.FilePath -> IO AttendeeDB
loadDB path = do
  m <- attendees <$> BS.readFile path
  return AttendeeDB {dbAttendees = m, dbPath = path}
  where
    attendees bs = case attendees' bs of
      A.Success v -> v
      _           -> error "invalid json"

    attendees' bs = case At.parseOnly A.json bs of
      Right v -> AT.parse A.parseJSON v
      Left e  -> A.Error e

saveDB :: AttendeeDB -> IO ()
saveDB db = LBS.writeFile (dbPath db) $ A.encode $ dbAttendees db

encrypt :: LBS.ByteString -> String
encrypt = SHA.showDigest . SHA.sha1
