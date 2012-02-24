{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp
  ) where

import qualified RicEvents.Database as D

import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import Text.XHtml ((<<), (+++), (</>), (<->), (!))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import qualified Control.Monad.Reader as R
import Control.Monad.Trans
import Control.Applicative

waiApp :: W.Application
waiApp req = htmlResponse <$> do
  message <- liftIO $ readFile "./message.txt"
  return $ render mainView RenderContext
    { rcAttendees = []
    , rcViewTitle = "hogefuga"
    , rcHeaderMessage = message
    , rcRequest = req
    }

htmlResponse :: H.Html -> W.Response
htmlResponse h
  = success [(HT.headerContentType "text/html")]
      $ LBS.fromChunks [BC.pack $ H.prettyHtml h]

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

type View = R.Reader RenderContext

render :: View H.Html -> RenderContext -> H.Html
render = R.runReader

mainView :: View H.Html
mainView = H.concatHtml <$> sequence [header, body]
  where
    header = do
      t <- title
      return $ H.header << (linkCSS +++ (H.thetitle << t))

    linkCSS
      = H.thelink !
          [ H.rel "stylesheet"
          , H.src "/ric-events.css"
          , H.thetype "text/css"
          ] << H.noHtml

    body = do
      h <- headerMessage
      t <- title
      return $ H.h1 << t +++ H.paragraph << h

    title = R.asks rcViewTitle

    headerMessage = R.asks rcHeaderMessage
