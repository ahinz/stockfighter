{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stockfighter.Order where

import qualified Stockfighter.Gamemaster as GM
import qualified Stockfighter.Data as D

import Data.Aeson
import Data.Aeson.Encode
import GHC.Generics


data CreateOrderRequest = CreateOrderRequest { account :: String
                                             , venue :: String
                                             , stock :: String
                                             , price :: D.StockPrice
                                             , qty :: Integer
                                             , direction :: D.Direction
                                             , orderType :: D.OrderType }
                        deriving (Show, Eq, Generic)

instance ToJSON D.Direction where
  toJSON D.Buy = String "buy"
  toJSON D.Sell = String "sell"

instance ToJSON D.OrderType where
  toJSON D.Market = String "market"
  toJSON D.Limit = String "limit"
  toJSON D.FireOrKill = String "fire-or-kill"
  toJSON D.ImmdOrCancel = String "immediate-or-cancel"

instance ToJSON D.StockPrice where
  toJSON (D.StockPrice s) = toJSON s

instance ToJSON CreateOrderRequest

createOrderReq :: GM.Level -> D.OrderType -> D.StockPrice -> Integer -> D.Direction  -> CreateOrderRequest
createOrderReq l typ price qty dir =
  CreateOrderRequest { account = GM.tradingAccount l
                     , venue  = GM.levelVenue l
                     , stock = GM.ticker l
                     , price = price
                     , qty = qty
                     , direction = dir
                     , orderType = typ }
