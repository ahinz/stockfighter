{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stockfighter.Exchange where

import Control.Applicative
import Data.Aeson
import GHC.Generics

import Network.HTTP.Simple

import Stockfighter.Http
import qualified Stockfighter.Order as Order
import qualified Stockfighter.Gamemaster as GM
import qualified Stockfighter.Data as D

import qualified Data.Text as T

data HeartbeatResponse = HeartbeatResponse { ok :: Bool
                                           , error :: String }
                       deriving (Generic, Show)

instance FromJSON HeartbeatResponse

heartbeat :: StockfighterApiKey -> IO (Either JSONException HeartbeatResponse)
heartbeat k =
  executeRequest $ apiRequest k "GET" path
  where
    path =  T.concat [apiBaseUrl, "/heartbeat"]


data VenueResponse = VenueResponse { venue :: String }
                   deriving (Generic, Show)

instance FromJSON VenueResponse

venueHeartbeat :: StockfighterApiKey -> String -> IO (Either JSONException VenueResponse)
venueHeartbeat k v =
  executeRequest $ apiRequest k "GET" path
  where
    path =  T.concat [apiBaseUrl, "/venues/", T.pack v, "/heartbeat"]

instance FromJSON D.Orderbook
instance FromJSON D.PriceQty

orderbook :: StockfighterApiKey -> String -> String -> IO (Either JSONException D.Orderbook)
orderbook k venue symbol =
  executeRequest $ apiRequest k "GET" path
  where
    path =  T.concat [apiBaseUrl, "/venues/", T.pack venue, "/stocks/", T.pack symbol]

createOrder :: StockfighterApiKey -> Order.CreateOrderRequest -> IO (Either JSONException Value)
createOrder k o =
  executeRequest $ setRequestBodyJSON o $ apiRequest k "POST" path
  where
    symbol = T.pack $ Order.stock o
    venue = T.pack $ Order.venue o
    path =  T.concat [apiBaseUrl, "/venues/", venue, "/stocks/", symbol, "/orders"]
