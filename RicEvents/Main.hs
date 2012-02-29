{-# LANGUAGE OverloadedStrings #-}

module RicEvents.Main
  ( waiApp
  ) where

import qualified RicEvents.Database as D
import qualified RicEvents.View as Vi
import qualified RicEvents.Config as Cf
import qualified RicEvents.Locale as L

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
import Data.Either
import Data.String
import Control.Monad.Trans
import Control.Monad
import Control.Applicative

waiApp :: Cf.Config -> W.Application
waiApp conf W.Request {W.requestMethod = m, W.requestBody = b, W.queryString = q}
  | m == "GET"  = handle renderTop Handler
    { hQuery = q
    , hConfig = conf
    }
  | m == "POST" = handle postHandler =<< do
    body <- b C.$$ CL.head
    return $ Handler
      { hQuery = HT.parseQuery $ fromMaybe "" body
      , hConfig = conf
      }
  | otherwise   = return errorResponse

data Handler = Handler
  { hQuery :: HT.Query
  , hConfig :: Cf.Config
  }

type HandlerM = R.ReaderT Handler (C.ResourceT IO)

handle :: HandlerM a -> Handler -> C.ResourceT IO a
handle = R.runReaderT

postHandler :: HandlerM W.Response
postHandler = do
  action <- query "action"
  case action of
    Just "new"    -> newResponse
    Just "delete" -> deleteResponse
    _             -> renderWithError [L.invalidRequest]
  where
    newResponse = do
      form <- inputForm
        <$> query "name"
        <*> query "circle"
        <*> query "comment"
        <*> query "password"
      d <- refConf Cf.cDatabasePath
      case validate form of
        Right attendee -> do
          _ <- liftIO $ D.withDB d $ D.putAttendee attendee
          return $ redirectResponse "/"
        Left errs -> renderWithError errs

    deleteResponse = do
      id_ <- queryDigit "id"
      p <- query "password"
      fromMaybe (renderWithError [L.noInput]) $ deleteAttendee <$> (BLC.pack <$> p) <*> id_

    deleteAttendee passwd id_ = do
      d <- refConf Cf.cDatabasePath
      status <- liftIO $ D.withDB d $ D.deleteAttendee passwd id_
      case status of
        Right _ -> return $ redirectResponse "/"
        Left _  -> renderWithError [L.invalidIdOrPassword]

renderTop :: HandlerM W.Response
renderTop = htmlResponse <$> renderTop' []

renderWithError :: [L.ErrorMessage] -> HandlerM W.Response
renderWithError errs = htmlResponse <$> renderTop' errs

renderTop' :: [L.ErrorMessage] -> HandlerM H.Html
renderTop' errs = do
  q <- httpQuery
  d <- refConf Cf.cDatabasePath
  h <- refConf Cf.cHeaderComment
  Right as <- liftIO $ D.withDB d D.getAllAttendees
  return $ Vi.render Vi.mainView Vi.RenderContext
    { Vi.rcAttendees = V.toList as
    , Vi.rcViewTitle = "hogefuga"
    , Vi.rcHeaderMessage = h
    , Vi.rcQuery = q
    , Vi.rcErrors = errs
    , Vi.rcLocale = L.localeJa
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

refConf :: (Cf.Config -> a) -> HandlerM a
refConf f = R.asks $ f . hConfig

htmlResponse :: H.Html -> W.Response
htmlResponse = W.responseLBS HT.status200
  [HT.headerContentType "text/html"] . fromString . H.prettyHtml

redirectResponse :: String -> W.Response
redirectResponse url = W.responseLBS HT.status301 [("Location", fromString url)] ""

errorResponse :: W.Response
errorResponse = W.responseLBS HT.status400
  [HT.headerContentType "text/plain"] "invalid request"

data AttendeeForm = AttendeeForm
  { aName :: Either L.ErrorMessage String
  , aCircle :: Either L.ErrorMessage String
  , aComment :: String
  , aPassword :: Either L.ErrorMessage String
  }

validate :: AttendeeForm -> Either [L.ErrorMessage] D.Attendee
validate AttendeeForm
  { aName = Right name
  , aCircle = Right circle
  , aComment = comment
  , aPassword = Right password
  }
  = Right $ D.mkAttendee name circle comment $ BLC.pack password
validate AttendeeForm
  { aName = name
  , aCircle = circle
  , aPassword = password
  }
  = Left $ lefts [name, circle, password]

inputForm :: Maybe String
          -> Maybe String
          -> Maybe String
          -> Maybe String
          -> AttendeeForm
inputForm name circle comment password
  = AttendeeForm (require L.missingName name)
                 (require L.missingCircle circle)
                 (norequire comment)
                 (require L.missingPassword password)
  where
    require msg (Just v)
      | null v    = Left msg
      | otherwise = Right v
    require msg _ = Left msg

    norequire = fromMaybe ""
