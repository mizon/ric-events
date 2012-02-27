{-# LANGUAGE OverloadedStrings #-}

import qualified RicEvents.Main as Main

import qualified Network.Wai.Handler.Warp as Wp
import qualified System.Environment as E
import qualified System.Console.GetOpt as O
import Control.Applicative

main :: IO ()
main = do
  p <- port
  putStrLn $ "serve at " ++ show p
  Wp.run p Main.waiApp
  where
    port = do
      (opts, _, _) <- O.getOpt O.Permute optSpec <$> E.getArgs
      return $ getPort opts

getPort :: [ServerOption] -> Int
getPort (PortNumber n:_) = n
getPort (_:os)           = getPort os
getPort _                = undefined

data ServerOption
  = PortNumber Int

optSpec :: [O.OptDescr ServerOption]
optSpec =
  [ O.Option ['p'] ["port"] (O.ReqArg (PortNumber . read) "PORT") "server port"
  ]
