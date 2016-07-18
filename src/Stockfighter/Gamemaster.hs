{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- https://discuss.starfighters.io/t/the-gm-api-how-to-start-stop-restart-resume-trading-levels-automagically/143

module Stockfighter.Gamemaster where

import Control.Applicative
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import Stockfighter.Http

import qualified Data.Text as T

data InstanceId = InstanceId Integer deriving (Show, Eq)

data StartLevelResponse = StartLevelResponse {
  ok :: Bool,
  instanceId :: Integer,
  secondsPerTradingDay :: Double,
  account :: String,
  tickers :: [String],
  venues :: [String] } deriving (Generic, Show)

data Level = Level {
  levelInstanceId :: InstanceId,
  tradingAccount :: String,
  ticker :: String,
  levelVenue :: String
  } deriving (Show, Eq)

instance FromJSON StartLevelResponse

iToText :: Integer -> T.Text
iToText = T.pack . show

startLevelUrl :: T.Text -> T.Text
startLevelUrl level = T.concat [gmBaseUrl, "/levels/", level]

startLevelRequest :: StockfighterApiKey -> T.Text -> Request
startLevelRequest k t =
  gmRequest k "POST" path
  where
    path = startLevelUrl t

stopLevelRequest :: StockfighterApiKey -> InstanceId -> Request
stopLevelRequest k (InstanceId i) =
  gmRequest k "POST" path
  where
    instanceId = iToText i
    path = T.concat [ gmBaseUrl
                    , "/instances/"
                    , instanceId
                    , "/stop"]

restartLevelUrl :: InstanceId -> T.Text
restartLevelUrl (InstanceId instanceId) =
  T.concat [gmBaseUrl
           , "/instances/"
           , instanceId'
           , "/restart"]
  where instanceId' = iToText instanceId

restartLevelRequest :: StockfighterApiKey -> InstanceId -> Request
restartLevelRequest k id =
  gmRequest k "POST" path
  where
    path = restartLevelUrl id

toLevel :: StartLevelResponse -> Level
toLevel a = Level { levelInstanceId = InstanceId $ instanceId a
                      , tradingAccount = account a
                      , ticker = head $ tickers a
                      , levelVenue = head $ venues a }

stopLevel :: StockfighterApiKey -> InstanceId -> IO (Either JSONException Value)
stopLevel k i =
  executeRequest $ stopLevelRequest k i

restartLevel :: StockfighterApiKey -> InstanceId -> IO (Either JSONException Level)
restartLevel k i =
  do
    stopLevel k i
    r <- executeRequest $ restartLevelRequest k i
    return $ toLevel <$> r

startLevel :: StockfighterApiKey -> T.Text -> IO (Either JSONException Level)
startLevel k t =
  do
    r <- executeRequest $ startLevelRequest k t
    return $ toLevel <$> r
