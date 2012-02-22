{-# LANGUAGE StandaloneDeriving #-}

import qualified RicEvents.Database as D
import qualified System.Directory as Di
import Test.HUnit
import Control.Monad
import System.Exit

import Debug.Trace

deriving instance Eq D.Attendee
deriving instance Show D.Attendee

main :: IO ()
main = do
  Counts {errors = e, failures = f} <- runTestTT checkDB
  when (e + f > 0)
    exitFailure

checkDB :: Test
checkDB
  = TestList
    [ "check save and load" ~: do
        cdir <- Di.getCurrentDirectory
        putStrLn cdir
        db <- D.loadDB dbPath
        D.saveDB $ D.put attendee db
        reload <- D.loadDB dbPath
        D.get "10" reload @?= Just attendee
    ]
  where
    dbPath = "./hoge.json"

    attendee = D.Attendee 10 "mizon" "ricora" "yattaneBokutin"
