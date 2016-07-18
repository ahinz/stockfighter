{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Stockfighter.OrderResponse where

import Prelude hiding (id)

import qualified Stockfighter.Gamemaster as GM
import qualified Stockfighter.Data as D

import Control.Applicative

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Encode
import Data.Aeson.Types
import GHC.Generics

data Order = Order { symbol :: String
                   , venue :: String
                   , direction :: D.Direction
                   , originalQty :: Integer
                   , qty :: Integer
                   , price :: D.StockPrice
                   , orderType :: D.OrderType
                   , id :: Integer
                   , fills :: [D.PriceQty] }
             deriving (Generic, Show, Eq)

mkDir :: T.Text -> D.Direction
mkDir "buy" = D.Buy
mkDir "sell" = D.Sell
mkDir _ = error "Invalid direction"

mkOrderType :: T.Text -> D.OrderType
mkOrderType "market" = D.Market
mkOrderType "limit" = D.Limit
mkOrderType "fire-or-kill" = D.FireOrKill
mkOrderType "immediate-or-cancel" = D.ImmdOrCancel
mkOrderType _ = error "Invalid order type"

instance FromJSON D.Direction where
  parseJSON (String s) = pure $ mkDir s

instance FromJSON D.OrderType where
  parseJSON (String s) = pure $ mkOrderType s

instance FromJSON Order
instance FromJSON D.PriceQty
instance FromJSON D.StockPrice

toRegularOrder :: Order -> D.FilledOrder
toRegularOrder Order {id, qty, price, orderType, direction, fills}
  = D.FilledOrder { D.order=D.Order { D.orderQty = qty
                                    , D.orderPrice = price
                                    , D.orderType = orderType
                                    , D.orderDir = direction
                                    , D.orderId = Just $ D.OrderId id }
                  , D.fills = map toFill fills}
    where
      toFill (D.PriceQty {D.price, D.qty}) = D.Fill { D.fillPrice = D.StockPrice $ floor price
                                                    , D.fillQty = qty }
