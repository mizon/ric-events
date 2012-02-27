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

    body = do
      t <- (H.h1 <<) <$> title
      h <- (H.paragraph <<) <$> R.asks rcHeaderMessage
      as <- attendees
      return $ H.concatHtml [t, h, as, newForm, deleteForm]

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

newForm :: H.Html
newForm = postForm "Register" inner
  where
    inner = H.input ! [H.thetype "hidden", H.name "action", H.value "new"]
        +++ H.defList
            [ inputText "attendee-name" "name"
            , inputText "attendee-circle" "circle"
            , inputText "attendee-comment" "comment"
            , inputPassword "attendee-password" "password"
            ]
        +++ submit "send"

    inputText = input "text"

    inputPassword = input "password"

    input type_ label dest =
      ( H.label ! [H.thefor label] << label
      , H.input ! [H.thetype type_, H.name dest, H.identifier label]
      )

deleteForm :: H.Html
deleteForm = postForm "Delete" inner
  where
    inner = H.input ! [H.thetype "hidden", H.name "action", H.value "delete"]
        +++ input "text" "attendee-id" "id"
        +++ input "password" "attendee-password" "password"
        +++ submit "send"

    input type_ label dest = H.label ! [H.thefor label] << label
                         +++ H.input ! [H.thetype type_, H.name dest, H.identifier label]

submit :: String -> H.Html
submit value = H.input ! [H.thetype "submit", H.value value]

postForm :: String -> H.Html -> H.Html
postForm title inner = H.h2 << title
                   +++ H.form ! [H.action "/", H.method "post"] << inner
