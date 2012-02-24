{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Wai.Handler.Warp as Wp
import qualified Network.Wai as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HT
import qualified System.Environment as E
import qualified System.Console.GetOpt as O
import Control.Applicative
import Control.Monad.Trans

main :: IO ()
main = do
  (opts, _, _) <- O.getOpt O.Permute optSpec <$> E.getArgs
  Wp.run (getPort opts) app

getPort :: [ServerOption] -> Int
getPort []               = undefined
getPort (PortNumber n:_) = n
getPort (_:os)           = getPort os

app :: W.Application
app req = do
  bs <- liftIO $ BS.readFile "./hoge.txt"
  return $ W.responseLBS HT.status200 headers
         $ LBS.fromChunks
           [ W.requestMethod req
           , BC.pack $ show $ W.httpVersion req
           , W.rawPathInfo req
           , bs
           ]
  where
    headers = [HT.headerContentType "text/plain"]

data ServerOption
  = PortNumber Int

optSpec :: [O.OptDescr ServerOption]
optSpec = [O.Option ['p'] ["port"] (O.ReqArg (PortNumber . read) "PORT") "server port"]
