{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

import qualified RicEvents.Database as D
import qualified System.Directory as Di
import qualified Data.ByteString as BS
import Test.HUnit
import Control.Monad
import System.Exit

import Debug.Trace

deriving instance Eq D.Attendee

main :: IO ()
main = do
  Counts {errors = e, failures = f} <- runTestTT checkDB
  when (e + f > 0)
    exitFailure

checkDB :: Test
checkDB
  = TestList
    [ "check save and load" ~: do
        cleanUp
        db <- D.loadDB dbPath
        D.saveDB $ D.put attendee db
        reload <- D.loadDB dbPath
        expectAt 0 attendee reload

    , "with db" ~: do
        cleanUp
        D.withDB dbPath $ do
          D.putAttendee attendee
          D.putAttendee attendee
          D.putAttendee attendee
        a <- D.withDB dbPath $ D.getAttendee 0
        a @?= Just attendee {D.aId = Just 0}
    ]
  where
    dbPath = "./hoge.json"

    expectAt i attendee db = D.get i db @?= Just attendee {D.aId = Just i}

    attendee = D.mkAttendee "mizon" "ricora" "yattaneBokutin"

    cleanUp = do
      Di.removeFile dbPath
      BS.writeFile dbPath "[]"
