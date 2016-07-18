{-# LANGUAGE DeriveGeneric #-}

module Stockfighter.Data where

import GHC.Generics

newtype StockPrice = StockPrice Integer
                   deriving (Show, Eq)

data Direction = Buy
               | Sell
               deriving (Show, Eq)

data OrderType = Market
               | Limit
               | FireOrKill
               | ImmdOrCancel
               deriving (Show, Eq)

data Fill = Fill { fillPrice :: StockPrice
                 , fillQty :: Integer
                 , fillDate :: String }
          deriving (Show, Eq)

newtype OrderId = OrderId Integer
                deriving (Show, Eq)

data Order = Order { orderQty :: Integer
                   , orderType :: OrderType
                   , orderDir :: Direction
                   , orderId :: Maybe OrderId }
           deriving (Show, Eq)

data FilledOrder = FilledOrder { order :: Order
                               , fills :: [Fill] }
                 deriving (Show, Eq)

data PriceQty = PriceQty { price :: Double
                         , qty :: Integer }
              deriving (Generic, Show, Eq)

data Orderbook= Orderbook { bids :: Maybe [PriceQty]
                          , asks :: Maybe [PriceQty] }
              deriving (Generic, Show, Eq)

data Quote = Quote { bidPrice :: StockPrice
                   , askPrice :: StockPrice }
             deriving (Show, Eq)
