{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Config
  ( Config(..), parseConfig
  ) where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import qualified System.FilePath as F
import Control.Applicative

parseConfig :: F.FilePath -> IO (Maybe Config)
parseConfig = Y.decodeFile

data Config = Config
  { cHeaderComment :: String
  , cPasswordSalt :: String
  , cDatabasePath :: F.FilePath
  }
  deriving (Show, Eq)

instance Y.FromJSON Config where
  parseJSON (Y.Object o)
    = Config <$> o .: "header-comment"
             <*> o .: "password-salt"
             <*> o .: "database-path"
