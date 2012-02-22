{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

import qualified RicEvents.Database as D
import qualified System.Directory as Di
import qualified Data.ByteString as BS
import Test.HUnit
import Control.Monad
import System.Exit

deriving instance Eq D.Attendee

main :: IO ()
main = do
  Counts {errors = e, failures = f} <- runTestTT checkDB
  when (e + f > 0)
    exitFailure

checkDB :: Test
checkDB
  = TestList
    [ "save and load" ~: do
        cleanUp
        db <- D.loadDB dbPath
        D.saveDB $ D.put attendee db
        reload <- D.loadDB dbPath
        expectAt 0 attendee reload

    , "put attendee" ~: do
        cleanUp
        D.withDB dbPath $ do
          D.putAttendee attendee
          D.putAttendee attendee
          D.putAttendee attendee
        as <- D.withDB dbPath $ sequence $ map D.getAttendee [0..2]
        forM_ [0..2] $ \i -> do
          as !! i @?= Just attendee {D.aId = Just i}

    , "delete attendee" ~: do
        cleanUp
        D.withDB dbPath $ do
          D.putAttendee attendee
          D.putAttendee attendee
        D.withDB dbPath $ do
          D.deleteAttendee 0
        (a1, a2) <- D.withDB dbPath $ do
           a <- D.getAttendee 0
           b <- D.getAttendee 1
           return (a, b)
        a1 @?= Nothing
        a2 @?= Just attendee {D.aId = Just 1}
     ]
  where
    dbPath = "./hoge.json"

    expectAt i attendee_ db = D.get i db @?= Just attendee_ {D.aId = Just i}

    attendee = D.mkAttendee "mizon" "ricora" "yattaneBokutin"

    cleanUp = do
      Di.removeFile dbPath
      BS.writeFile dbPath "[]"
