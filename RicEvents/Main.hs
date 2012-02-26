{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp
  ) where

import qualified RicEvents.Database as D

import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import Text.XHtml ((<<), (+++), (</>), (<->), (!))
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Control.Monad.Reader as R
import Data.Maybe
import Data.String
import Control.Monad.Trans
import Control.Monad
import Control.Applicative

waiApp :: W.Application
waiApp W.Request {W.requestMethod = m, W.requestBody = b, W.queryString = q}
  | m == "GET"  = handle hGET q
  | m == "POST" = handle hPOST =<< queryStream
  | otherwise   = return errorResponse
  where
    queryStream = do
      body <- b C.$$ CL.head
      return $ HT.parseQuery $ fromMaybe "" body

type Handler = R.Reader HT.Query

handle :: Handler (C.ResourceT IO W.Response) -> HT.Query -> C.ResourceT IO W.Response
handle = R.runReader

hGET :: Handler (C.ResourceT IO W.Response)
hGET = do
  q <- R.ask
  return $ htmlResponse <$> do
    message <- liftIO $ readFile "./message.txt"
    as <- liftIO $ V.toList <$> D.withDB "./hoge.json" D.getAllAttendees
    return $ render mainView RenderContext
      { rcAttendees = as
      , rcViewTitle = "hogefuga"
      , rcHeaderMessage = message
      , rcQuery = q
      }

hPOST :: Handler (C.ResourceT IO W.Response)
hPOST = do
  action <- query "action"
  case action of
    Just "new" -> newResponse
    _          -> invalidQuery
  where
    newResponse = do
      form <- AttendeeForm <$> query "name" <*> query "circle" <*> query "comment"
      case validate form of
        Just attendee -> return $ do
          liftIO $ D.withDB "./hoge.json" $ D.putAttendee attendee
          return $ redirectResponse "/"
        Nothing -> invalidQuery

    invalidQuery = return $ return errorResponse

query :: String -> Handler (Maybe String)
query key = do
  v <- R.asks $ lookup $ fromString key
  return $ BC.unpack <$> join v

htmlResponse :: H.Html -> W.Response
htmlResponse = W.responseLBS HT.status200
  [HT.headerContentType "text/html"] . fromString . H.prettyHtml

redirectResponse :: String -> W.Response
redirectResponse url = W.responseLBS HT.status301 [("Location", fromString url)] ""

errorResponse :: W.Response
errorResponse = W.responseLBS HT.status400
  [HT.headerContentType "test/plain"] "invalid request"

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

data RenderContext = RenderContext
  { rcAttendees :: [D.Attendee]
  , rcViewTitle :: String
  , rcHeaderMessage :: String
  , rcQuery :: HT.Query
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
  return $ H.form ! [H.action "/", H.method "post"] << inner
  where
    inner = H.input ! [H.thetype "hidden", H.name "action", H.value "new"]
        +++ H.defList
            [ inputText "attendee-name" "name"
            , inputText "attendee-circle" "circle"
            , inputText "attendee-comment" "comment"
            , inputPassword "attendee-password" "password"
            ]
        +++ H.input ! [H.thetype "submit", H.value "send"]

    inputText = input "text"

    inputPassword = input "password"

    input type_ label dest =
      ( H.label ! [H.thefor label] << label
      , H.input ! [H.thetype type_, H.name dest, H.identifier label]
      )
