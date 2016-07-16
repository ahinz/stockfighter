{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- https://discuss.starfighters.io/t/the-gm-api-how-to-start-stop-restart-resume-trading-levels-automagically/143

module Stockfighter where

import GHC.Generics

import System.Environment
import Data.Aeson

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Simple
import qualified Network.HTTP.Client.TLS as H
import qualified Network.HTTP.Client as H

data StockfighterApiKey  = StockfighterApiKey T.Text
data InstanceId = InstanceId Integer

data StartLevelResponse = StartLevelResponse {
  ok :: Bool,
  instanceId :: Integer,
  secondsPerTradingDay :: Double,
  account :: String,
  tickers :: [String],
  venues :: [String] } deriving (Generic, Show)

data Level = Level {
  levelInstanceId :: Integer,
  tradingAccount :: String,
  ticker :: String,
  venue :: String
  } deriving (Show, Eq)

instance FromJSON StartLevelResponse

gmHost = "www.stockfighter.io"
gmBaseUrl = "/gm"

iToText :: Integer -> T.Text
iToText = T.pack . show

startLevelUrl :: T.Text -> T.Text
startLevelUrl level = T.concat [gmBaseUrl, "/levels/", level]

baseRequest :: StockfighterApiKey -> S8.ByteString -> T.Text -> Request
baseRequest (StockfighterApiKey k) method path =
  setRequestPath path'
  $ setRequestMethod method
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestHeader "Cookie" [cookie]
  $ defaultRequest
  where
    cookie = Enc.encodeUtf8 $ T.concat ["api_key=", k]
    path' = Enc.encodeUtf8 path

gmRequest :: StockfighterApiKey -> S8.ByteString -> T.Text -> Request
gmRequest k m p =
  setRequestHost gmHost $ baseRequest k m p

startLevelRequest :: StockfighterApiKey -> T.Text -> Request
startLevelRequest k t =
  gmRequest k "POST" path
  where
    path = startLevelUrl t

restartLevelUrl :: Level -> T.Text
restartLevelUrl l = T.concat [gmBaseUrl
                             , "/instances/"
                             , instanceId
                             , "/restart"]
  where instanceId = iToText $ levelInstanceId l

restartLevelRequest :: StockfighterApiKey -> Level -> Request
restartLevelRequest k l =
  gmRequest k "POST" path
  where
    path = restartLevelUrl l

setDefaultTimeout :: H.Request -> H.Request
setDefaultTimeout r =
  r { H.responseTimeout = H.responseTimeoutMicro 120000000 }

restartLevel :: StockfighterApiKey -> Level -> IO (Either JSONException Value)
restartLevel k l =
  let request = restartLevelRequest k l in
  do
    manager <- H.newManager H.tlsManagerSettings
    response <- httpJSONEither (setRequestManager manager
                                $ setDefaultTimeout request)
    return $ getResponseBody response

convert :: Either a (IO b) -> IO (Either a b)
convert = either (return . Left) (fmap Right)

startLevel :: StockfighterApiKey -> T.Text -> IO (Either JSONException Level)
startLevel k t =
  let request = startLevelRequest k t in
  do
    response <- httpJSONEither request
    return $ toLevel <$> getResponseBody response
  where
    toLevel a = Level { levelInstanceId = instanceId a
                      , tradingAccount = account a
                      , ticker = head $ tickers a
                      , venue = head $ venues a }
