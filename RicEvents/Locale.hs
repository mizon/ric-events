module RicEvents.Locale
  ( Locale(..), localeJa, ErrorMessage
  ) where

data Locale = Locale
  { missingName :: String
  , missingCircle :: String
  , missingPassword :: String
  , invalidRequest :: String
  , noInput :: String
  , invalidIdOrPassword :: String
  }

localeJa :: Locale
localeJa = Locale
  { missingName = "名前が入力されていません。"
  , missingCircle = "サークル名が入力されていません。"
  , missingPassword = "パスワードが入力されていません。"
  , invalidRequest = "invalid request"
  , noInput = "入力が空です。"
  , invalidIdOrPassword = "idもしくはパスワードが空です。"
  }

type ErrorMessage = Locale -> String
