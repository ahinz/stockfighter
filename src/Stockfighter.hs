{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stockfighter where

import Stockfighter.Gamemaster
import Stockfighter.Exchange
import Stockfighter.Data
import Stockfighter.Http

import Network.HTTP.Simple
import Control.Concurrent
import Debug.Trace

import qualified Data.Text as T

data OrderBookDelta = OrderBookDelta { newBids :: [PriceQty]
                                     , newAsks :: [PriceQty] }
                    deriving (Show, Eq)

data Event = InitEvent
           | OrderFilled [FilledOrder]
           | QuoteUpdated
           | OrderBookUpdated

data Action = PlaceOrder Order
            | CancelOrder OrderId
            deriving (Show, Eq)

data Position = Position { shares :: Integer
                         , cash :: Integer }
              deriving (Show, Eq)

data LevelState = LevelState { orderBook :: Orderbook
                             , lastQuote :: PriceQty
                             , openOrders :: [FilledOrder]
                             , position :: Position }
                deriving (Show, Eq)

type Simulation a = LevelState -> Event -> a -> (a, [Action], Bool)

data LevelResult = JsonError JSONException
                 | Success
                 deriving Show

defaultLevelState :: LevelState
defaultLevelState = LevelState { orderBook = Orderbook { bids = Just []
                                                       , asks = Just [] }
                               , lastQuote = PriceQty { price = 0
                                                      , qty = 0 }
                               , openOrders = []
                               , position = Position { shares = 0
                                                     , cash = 0 }}


runLevel :: StockfighterApiKey -> T.Text -> Simulation a -> a -> IO LevelResult
runLevel key levelName simulation a = do
  levelOrError <- startLevel key levelName
  case levelOrError of
    Right level -> runLevel' key (levelInstanceId level) simulation a
    Left jsonError -> return $ JsonError jsonError

runLevel' :: StockfighterApiKey -> InstanceId -> Simulation a -> a -> IO LevelResult
runLevel' k instanceId simFn a = do
  levelOrError <- restartLevel k instanceId
  case levelOrError of
    Right level -> simLevel k level simFn a
    Left jsonError -> return $ JsonError jsonError

data UpdateEvent = NewOrderBook Orderbook
                 | NewQuote PriceQty
                 | ApiError JSONException
                 deriving Show

orderbookPollingLoop :: StockfighterApiKey -> Level -> MVar UpdateEvent -> IO ()
orderbookPollingLoop k l mv = do
  ob <- orderbook k (levelVenue l) (ticker l)

  putMVar mv $ case ob of
    Right orderBook -> NewOrderBook orderBook
    Left jsonError -> ApiError jsonError

  threadDelay 1000
  orderbookPollingLoop k l mv

updatePosition :: LevelState -> [FilledOrder] -> LevelState
updatePosition ls filledOrders =
  case (position ls, sumDeltas $ map toDelta filledOrders) of
     (p, delta) -> ls { position = addToPosition p delta }

  where
    orderToFills fo = (orderDir $ order fo, fills fo)
    toDelta fo = case orderToFills fo of
      (Buy, fills) -> (addShares fills 1,
                       addCash fills (-1))
      (Sell, fills) -> (addShares fills (-1),
                        addCash fills 1)

    sumDelta (s1, p1) (s2, p2) = (s1 + s2, p1 + p2)
    sumDeltas = foldl sumDelta (0, 0)

    addToPosition p (shareDelta, cashDelta) =
      Position { shares = (shares p) + shareDelta
               , cash = (cash p) + cashDelta}

    sum = foldl (+) 0
    qty = fillQty
    price f = case fillPrice f of
      StockPrice p -> p
    cashd f = (qty f) * (price f)

    addShares fills n = n * (sum $ map qty fills)
    addCash fills n = n * (sum $ map cashd fills)

runAction :: Action -> IO FilledOrder
runAction (PlaceOrder o) =

  return $ FilledOrder { order=o, fills = []}
--runAction (CancelOrder o) = return $ FilledOrder { order=o, fills = []}

runActions :: [Action] -> IO [FilledOrder]
runActions actions =

  return []

actionLoop :: LevelState -> Simulation a -> a -> [Action] -> IO (Either LevelResult LevelState)
actionLoop initState sim simState actions = do
  filledOrders <- runActions actions
  case filledOrders of
    [] -> return $ Right initState
    fills -> case sim levelState event simState of
      (_, _, True) -> return $ Left Success
      (newSimState, actions, False) -> actionLoop levelState sim newSimState actions
      where
        levelState = updatePosition initState fills
        event = OrderFilled fills


executeLoop :: MVar UpdateEvent -> LevelState -> Simulation a -> a -> IO LevelResult
executeLoop mv ls s init = do
  ue <- takeMVar mv
  case updateSim ue of
    Right (_, (_, _, True)) -> return Success
    Right (newLevelState, (newState, actions, False)) -> executeLoop mv newLevelState s newState
    Left e -> return e
  where
    nextLevelState ue = case ue of
      NewOrderBook ob -> Right (ls { orderBook = ob }, OrderBookUpdated)
      NewQuote pq -> Right $ (ls { lastQuote = pq }, QuoteUpdated)
      ApiError er -> Left $ JsonError er

    updateSim ue = case nextLevelState ue of
      Right (levelState, event) -> Right $ (levelState, s levelState event init)
      Left a -> Left a


simLevel :: StockfighterApiKey -> Level -> Simulation a -> a -> IO LevelResult
simLevel k l s a = do
  comm <- newEmptyMVar

  forkIO $ orderbookPollingLoop k l comm

  executeLoop comm defaultLevelState s a
