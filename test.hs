{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

import qualified RicEvents.Database as D
import qualified RicEvents.Config as C

import qualified System.Directory as Di
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 ()
import qualified Data.Vector as V
import Test.HUnit
import Control.Monad
import System.Exit

deriving instance Eq D.Attendee

main :: IO ()
main = do
  Counts {errors = e, failures = f} <- runTestTT checkAll
  when (e + f > 0)
    exitFailure

checkAll :: Test
checkAll = TestList [checkDB, checkConfig]

checkDB :: Test
checkDB = TestList
  [ "save and load" ~: do
      cleanUp
      db <- D.loadDB dbPath
      D.saveDB $ D.put attendee db
      reload <- D.loadDB dbPath
      expectAt 0 attendee reload

  , "put attendee" ~: do
      cleanUp
      withDBSuppress dbPath $ do
        D.putAttendee attendee
        D.putAttendee attendee
        D.putAttendee attendee
      Right as <- D.withDB dbPath $ sequence $ map D.getAttendee [0..2]
      forM_ [0..2] $ \i -> do
        as !! i @?= Just attendee {D.aId = Just i}

  , "delete attendee" ~: do
      cleanUp
      withDBSuppress dbPath $ do
        D.putAttendee attendee
        D.putAttendee attendee
      withDBSuppress dbPath $ do
        D.deleteAttendee "pass" 0
      Right (a1, a2) <- D.withDB dbPath $ do
        a <- D.getAttendee 0
        b <- D.getAttendee 1
        return (a, b)
      a1 @?= Nothing
      a2 @?= Just attendee {D.aId = Just 1}

  , "all" ~: do
      cleanUp
      withDBSuppress dbPath $ do
        D.putAttendee attendee
        D.putAttendee attendee
        D.putAttendee attendee
      Right all_ <- D.withDB dbPath $ do
        D.deleteAttendee "pass" 0
        D.getAllAttendees
      V.length all_ @?= 2
  ]
  where
    withDBSuppress path_ action = do
      _ <- D.withDB path_ action
      return ()

    dbPath = "./hoge.json"

    expectAt i attendee_ db = D.get i db @?= Just attendee_ {D.aId = Just i}

    attendee = D.mkAttendee "mizon" "ricora" "yattaneBokutin" "pass"

    cleanUp = do
      Di.removeFile dbPath
      BS.writeFile dbPath "[]"

checkConfig :: Test
checkConfig = TestList
  [ "parse file" ~: do
      Just conf <- C.parseConfig "config-test.yaml"
      conf @?= expect
  ]
  where
    expect = C.Config
      { C.cHeaderComment = "message"
      , C.cPasswordSalt = "salt"
      , C.cDatabasePath = "db-path"
      }
