module RicEvents.Config
  ( Config(..)
  ) where

import qualified System.FilePath as F

parseConfig :: F.FilePath -> IO Config
parseConfig = undefined

data Config = Config
  { cHeaderMessage :: String
  , cPasswordSalt :: String
  }
