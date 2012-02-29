{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp
  ) where

import qualified RicEvents.Database as D
import qualified RicEvents.View as Vi
import qualified RicEvents.Config as Cf

import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import qualified Data.ByteString.Char8 as BC
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

waiApp :: Cf.Config -> W.Application
waiApp conf W.Request {W.requestMethod = m, W.requestBody = b, W.queryString = q}
  | m == "GET"  = handle hGET Handler
    { hQuery = q
    , hConfig = conf
    }
  | m == "POST" = handle hPOST =<< postHandler
  | otherwise   = return errorResponse
  where
    postHandler = do
      body <- b C.$$ CL.head
      return $ Handler
        { hQuery = HT.parseQuery $ fromMaybe "" body
        , hConfig = conf
        }

data Handler = Handler
  { hQuery :: HT.Query
  , hConfig :: Cf.Config
  }

type HandlerM = R.Reader Handler

handle :: HandlerM (C.ResourceT IO W.Response) -> Handler -> C.ResourceT IO W.Response
handle = R.runReader

hGET :: HandlerM (C.ResourceT IO W.Response)
hGET = do
  h <- renderTop []
  return $ htmlResponse <$> liftIO h

hPOST :: HandlerM (C.ResourceT IO W.Response)
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
          _ <- liftIO $ D.withDB "./hoge.json" $ D.putAttendee attendee
          return $ redirectResponse "/"
        Nothing -> invalidQuery

    deleteResponse = do
      id_ <- queryDigit "id"
      p <- query "password"
      fromMaybe invalidQuery $ deleteAttendee <$> (BLC.pack <$> p) <*> id_

    deleteAttendee passwd id_ = return $ do
      status <- liftIO $ D.withDB "./hoge.json" $ D.deleteAttendee passwd id_
      return $ case status of
        Right _ -> redirectResponse "/"
        Left _  -> errorResponse

    invalidQuery = return $ return errorResponse

renderTop :: [String] -> HandlerM (IO H.Html)
renderTop errs = do
  q <- httpQuery
  return $ do
    message <- readFile "./message.txt"
    Right as <- D.withDB "./hoge.json" D.getAllAttendees
    return $ Vi.render Vi.mainView Vi.RenderContext
      { Vi.rcAttendees = V.toList as
      , Vi.rcViewTitle = "hogefuga"
      , Vi.rcHeaderMessage = message
      , Vi.rcQuery = q
      }

query :: String -> HandlerM (Maybe String)
query key = do
  q <- httpQuery
  return $ BC.unpack <$> (join $ lookup (fromString key) q)

queryDigit :: String -> HandlerM (Maybe Int)
queryDigit key = (toInt =<<) <$> query key
  where
    toInt str = case P.runP (P.many1 P.digit) [] [] str of
      Right v -> Just $ read v
      _       -> Nothing

httpQuery :: HandlerM HT.Query
httpQuery = R.asks hQuery

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
