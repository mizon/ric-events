{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp
  ) where

import qualified RicEvents.Database as D
import qualified RicEvents.View as Vi

import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as V
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Control.Monad.Reader as R
import qualified Text.Parsec as P
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
    return $ Vi.render Vi.mainView Vi.RenderContext
      { Vi.rcAttendees = as
      , Vi.rcViewTitle = "hogefuga"
      , Vi.rcHeaderMessage = message
      , Vi.rcQuery = q
      }

hPOST :: Handler (C.ResourceT IO W.Response)
hPOST = do
  action <- query "action"
  case action of
    Just "new"    -> newResponse
    Just "delete" -> deleteResponse
    _             -> invalidQuery
  where
    newResponse = do
      form <- AttendeeForm <$> query "name"
                           <*> query "circle"
                           <*> query "comment"
                           <*> query "password"
      case validate form of
        Just attendee -> return $ do
          liftIO $ D.withDB "./hoge.json" $ D.putAttendee attendee
          return $ redirectResponse "/"
        Nothing -> invalidQuery

    deleteResponse = do
      id_ <- queryDigit "id"
      p <- query "password"
      fromMaybe invalidQuery $ deleteAttendee <$> (BLC.pack <$> p) <*> id_

    deleteAttendee passwd id_ = return $ do
      liftIO $ D.withDB "./hoge.json" $ D.deleteAttendee passwd id_
      return $ redirectResponse "/"

    invalidQuery = return $ return errorResponse

query :: String -> Handler (Maybe String)
query key = do
  v <- R.asks $ lookup $ fromString key
  return $ BC.unpack <$> join v

queryDigit :: String -> Handler (Maybe Int)
queryDigit key = (toInt =<<) <$> query key
  where
    toInt str = case P.runP (P.many1 P.digit) [] [] str of
      Right v -> Just $ read v
      _       -> Nothing

htmlResponse :: H.Html -> W.Response
htmlResponse = W.responseLBS HT.status200
  [HT.headerContentType "text/html"] . fromString . H.prettyHtml

redirectResponse :: String -> W.Response
redirectResponse url = W.responseLBS HT.status301 [("Location", fromString url)] ""

errorResponse :: W.Response
errorResponse = W.responseLBS HT.status400
  [HT.headerContentType "text/plain"] "invalid request"

data AttendeeForm = AttendeeForm
  { aName :: Maybe String
  , aCircle :: Maybe String
  , aComment :: Maybe String
  , aPassword :: Maybe String
  }

validate :: AttendeeForm -> Maybe D.Attendee
validate AttendeeForm
  { aName = Just name
  , aCircle = Just circle
  , aComment = comment
  , aPassword = Just password
  }
  | any null [name, circle, password]
    = Nothing
  | otherwise
    = Just $ D.mkAttendee name circle (fromMaybe "" comment) $ BLC.pack password
validate _ = Nothing
