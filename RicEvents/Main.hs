{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp
  ) where

import qualified RicEvents.Database as D

import qualified Network.CGI as C
import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import qualified Text.XHtml.Strict as HS
import Text.XHtml ((<<), (+++), (</>), (<->))
import qualified Control.Monad.Reader as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans
import Control.Monad
import Control.Applicative

waiApp :: W.Application
waiApp req = do
  return $ htmlResponse html
  where
    html = render mainView RenderContext
      { rcAttendees = []
      , rcViewTitle = "hogefuga"
      , rcHeaderMessage = "konnitiaha"
      , rcRequest = req
      }

htmlResponse :: H.Html -> W.Response
htmlResponse h
  = success [(HT.headerContentType "text/html")]
      $ LBS.fromChunks [BC.pack $ H.showHtml h]

plainResponse :: String -> W.Response
plainResponse str
  = success [(HT.headerContentType "text/plain")] $ LBS.fromChunks [BC.pack str]

success :: HT.ResponseHeaders -> LBS.ByteString -> W.Response
success = W.responseLBS HT.status200

data RenderContext
  = RenderContext
    { rcAttendees :: [D.Attendee]
    , rcViewTitle :: String
    , rcHeaderMessage :: String
    , rcRequest :: W.Request
    }

type View = R.Reader RenderContext H.Html

render :: View -> RenderContext -> H.Html
render = R.runReader

mainView :: View
mainView = H.concatHtml <$> sequence [header, body]
  where
    header = return $ H.header << H.thetitle << title

    body = return
         $ H.h1 << title
       +++ H.paragraph << ("hogefuga, nuga" :: String)

    title = "this is ric-events script" :: String
