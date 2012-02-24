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
import qualified Data.Vector as V
import qualified Control.Monad.Reader as R
import Data.Maybe
import Control.Monad.Trans
import Control.Applicative

waiApp :: W.Application
waiApp req = htmlResponse <$> do
  message <- liftIO $ readFile "./message.txt"
  as <- liftIO $ V.toList <$> D.withDB "./hoge.json" D.getAllAttendees
  return $ render mainView RenderContext
    { rcAttendees = as
    , rcViewTitle = "hogefuga"
    , rcHeaderMessage = message
    , rcRequest = req
    }

htmlResponse :: H.Html -> W.Response
htmlResponse h = success "text/html" $ LBS.fromChunks [BC.pack $ H.prettyHtml h]

plainResponse :: String -> W.Response
plainResponse str = success "text/plain" $ LBS.fromChunks [BC.pack str]

success :: HT.Ascii -> LBS.ByteString -> W.Response
success ctype = W.responseLBS HT.status200 [(HT.headerContentType ctype)]

data RenderContext = RenderContext
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

    linkCSS = H.thelink !
      [ H.rel "stylesheet"
      , H.src "/ric-events.css"
      , H.thetype "text/css"
      ] << H.noHtml

    body = H.concatHtml <$> sequence
      [ (H.h1 <<) <$> title
      , (H.paragraph <<) <$> R.asks rcHeaderMessage
      , (H.paragraph <<) <$> attendees
      ]

    title = R.asks rcViewTitle

attendees :: View H.Html
attendees = do
  as <- R.asks rcAttendees
  return $ H.table <<
    if length as > 0 then
      foldl1 (</>) $ map attendeeToTr as
    else
      H.cell H.noHtml

attendeeToTr :: D.Attendee -> H.HtmlTable
attendeeToTr a
  = H.cell $ tdMaybe D.aId
         <-> td D.aName
         <-> td D.aCircle
         <-> td D.aComment
  where
    tdMaybe f = H.cell $ H.td << (fromMaybe "" $ show <$> f a)

    td f = H.cell $ H.td << f a
