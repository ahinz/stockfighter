{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stockfighter.Http where

import System.Environment
import Data.Aeson

import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Simple
import qualified Network.HTTP.Client.TLS as H
import qualified Network.HTTP.Client as H

data StockfighterApiKey  = StockfighterApiKey T.Text

apiHost = "api.stockfighter.io"

apiBaseUrl :: T.Text
apiBaseUrl = "/ob/api"

gmHost = "www.stockfighter.io"

gmBaseUrl :: T.Text
gmBaseUrl = "/gm"

baseRequest :: StockfighterApiKey -> S8.ByteString -> T.Text -> Request
baseRequest (StockfighterApiKey k) method path =
  setRequestPath path'
  $ setRequestMethod method
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestHeader "Cookie" [cookie]
  $ setRequestHeader "X-Starfighter-Authorization" [Enc.encodeUtf8 k]
  $ defaultRequest
  where
    cookie = Enc.encodeUtf8 $ T.concat ["api_key=", k]
    path' = Enc.encodeUtf8 path

gmRequest :: StockfighterApiKey -> S8.ByteString -> T.Text -> Request
gmRequest k m p =
  setRequestHost gmHost $ baseRequest k m p

apiRequest :: StockfighterApiKey -> S8.ByteString -> T.Text -> Request
apiRequest k m p =
  setRequestHost apiHost $ baseRequest k m p

setDefaultTimeout :: H.Request -> H.Request
setDefaultTimeout r =
  r { H.responseTimeout = H.responseTimeoutMicro 120000000 }

executeRequest :: FromJSON a => Request -> IO (Either JSONException a)
executeRequest request =
  do
    manager <- H.newManager H.tlsManagerSettings
    response <- httpJSONEither (setRequestManager manager
                                $ setDefaultTimeout request)
    return $ getResponseBody response

convert :: Either a (IO b) -> IO (Either a b)
convert = either (return . Left) (fmap Right)
