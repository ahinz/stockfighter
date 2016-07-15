{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stockfighter where

import GHC.Generics

import System.Environment
import Data.Aeson

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

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

restartLevelUrl (InstanceId i) = T.concat [gmBaseUrl, i', "/restart"]
  where i' = iToText i

startLevelUrl :: T.Text -> T.Text
startLevelUrl level = T.concat [gmBaseUrl, "/levels/", level]

startLevelReq :: StockfighterApiKey -> T.Text -> Request
startLevelReq (StockfighterApiKey k) t =
  setRequestPath path
  $ setRequestMethod "POST"
  $ setRequestHost gmHost
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestHeader "Cookie" [cookie]
  $ defaultRequest
  where
    cookie = Enc.encodeUtf8 $ T.concat ["api_key=", k]
    path = Enc.encodeUtf8 . startLevelUrl $ t

startLevel :: StockfighterApiKey -> T.Text -> IO (Either JSONException Level)
startLevel k t =
  let request = startLevelReq k t in
  do
    response <- httpJSONEither request
    return $ toLevel <$> getResponseBody response
  where
    toLevel a = Level { levelInstanceId = instanceId a
                      , tradingAccount = account a
                      , ticker = head $ tickers a
                      , venue = head $ venues a }


main' :: StockfighterApiKey -> IO (StartLevelResponse)
main' k =
  let request = startLevelReq k "first_steps" in
    do
      response <- httpJSON request

      putStrLn $ "The status code was: " ++
        show (getResponseStatusCode response)
      print $ getResponseHeader "Content-Type" response
      return $ getResponseBody response
