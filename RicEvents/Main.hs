{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp, waiApp2
  ) where

import qualified RicEvents.Database as D

import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import Text.XHtml ((<<), (+++), (</>), (<->), (!))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import Data.Conduit (runResourceT, ($$))
import qualified Data.Conduit.List as CL
import qualified Control.Monad.Reader as R
import Data.Maybe
import Data.String
import Control.Monad.Trans
import Control.Monad
import Control.Applicative

import Debug.Trace

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

waiApp2 :: W.Application
waiApp2 req@W.Request {W.requestMethod = m, W.requestBody = b}
  | m == "GET"  = handleGET
  | m == "POST" = handlePOST
  | otherwise   = return errorResponse
  where
    handleGET = htmlResponse <$> do
      message <- liftIO $ readFile "./message.txt"
      as <- liftIO $ V.toList <$> D.withDB "./hoge.json" D.getAllAttendees
      return $ render mainView RenderContext
        { rcAttendees = as
        , rcViewTitle = "hogefuga"
        , rcHeaderMessage = message
        , rcRequest = req
        }

    handlePOST = toResponse =<< postQuery "action"

    toResponse (Just "new") = do
      form <- AttendeeForm
          <$> postQuery "name"
          <*> postQuery "circle"
          <*> postQuery "comment"
      case validate form of
        Just attendee -> do
          liftIO $ D.withDB "./hoge.json" $ D.putAttendee attendee
          return $ redirectResponse "/"
        Nothing ->
          return $ errorResponse
    toResponse Nothing = return errorResponse

    postQuery key = do
      value <- join <$> postQuery' key
      return $ BC.unpack <$> value

    postQuery' key = do
      b_ <- liftIO requestBody
      return $ lookup key =<< HT.parseQuery <$> b_

    requestBody = runResourceT (b $$ CL.head)

data AttendeeForm = AttendeeForm
  { aName :: Maybe String
  , aCircle :: Maybe String
  , aComment :: Maybe String
  }

validate :: AttendeeForm -> Maybe D.Attendee
validate AttendeeForm
  { aName = Just name
  , aCircle = Just circle
  , aComment = comment
  }
  | null name || null circle
      = Nothing
  | otherwise
      = Just $ D.mkAttendee name circle $ fromMaybe "" comment
validate _ = Nothing

htmlResponse :: H.Html -> W.Response
htmlResponse = success "text/html" . fromString . H.prettyHtml

redirectResponse :: String -> W.Response
redirectResponse url = W.responseLBS HT.status301 [("Location", fromString url)] ""

plainResponse :: String -> W.Response
plainResponse = success "text/plain" . fromString

errorResponse :: W.Response
errorResponse = W.responseLBS HT.status400 [] "invalid request"

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
      , (H.paragraph <<) <$> newForm
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

newForm :: View H.Html
newForm = do
  return $ H.form ! [H.action "/", H.method "post"] << innerForm
  where
    innerForm = H.input ! [H.thetype "hidden", H.name "action", H.value "new"]
            +++ H.input ! [H.thetype "hidden", H.name "hoge", H.value "hogefuga"]
            +++ H.input ! [H.thetype "submit", H.value "send"]
