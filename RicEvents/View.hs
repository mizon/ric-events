module RicEvents.View
  ( mainView, render, RenderContext(..)
  ) where

import qualified RicEvents.Database as D

import qualified Network.HTTP.Types as HT
import qualified Text.XHtml as H
import Text.XHtml ((<<), (+++), (</>), (<->), (!))
import qualified Control.Monad.Reader as R
import Control.Applicative
import Data.Maybe

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
      , (H.paragraph <<) <$> return deleteForm
      ]

    title = R.asks rcViewTitle

data RenderContext = RenderContext
  { rcAttendees :: [D.Attendee]
  , rcViewTitle :: String
  , rcHeaderMessage :: String
  , rcQuery :: HT.Query
  }

type View = R.Reader RenderContext

render :: View H.Html -> RenderContext -> H.Html
render = R.runReader

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

deleteForm :: H.Html
deleteForm = H.h2 << ("Delete Attendee" :: String)
         +++ H.form ! [H.action "/", H.method "post"] << inner
  where
    inner = H.input ! [H.thetype "hidden", H.name "action", H.value "delete"]
        +++ input "text" "attendee-id" "id"
        +++ H.input ! [H.thetype "submit", H.value "send"]

    input type_ label dest = H.label ! [H.thefor label] << label
                         +++ H.input ! [H.thetype type_, H.name dest, H.identifier label]
