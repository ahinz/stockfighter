{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Stockfighter where

import Stockfighter.Gamemaster
import Stockfighter.Exchange
import Stockfighter.Data
import Stockfighter.Http

import qualified Stockfighter.Order as O

import Network.HTTP.Simple
import Control.Concurrent
import Control.Applicative
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

runAction :: StockfighterApiKey -> Level -> Action -> IO (Either JSONException FilledOrder)
runAction k l (PlaceOrder (Order {orderQty, orderType, orderDir, orderPrice})) =
  createOrder k cro
  where
    cro = O.createOrderReq l orderType orderPrice orderQty orderDir

runActions :: StockfighterApiKey -> Level -> [Action] -> IO (Either LevelResult [FilledOrder])
runActions k l actions = do
  actions' <- processActions actions
  return $ foldl reducer (Right []) actions'
  where
    reducer (Left l) _ = Left l
    reducer _ (Left j) = Left $ JsonError j
    reducer (Right fills) (Right fill) = Right (fill : fills)

    processActions actions = sequence $ map (runAction k l) actions

applyActions :: StockfighterApiKey -> Level -> LevelState -> Simulation a -> a -> [Action] -> IO (Either LevelResult (LevelState, a))
applyActions k l initState sim simState actions = do
  filledOrders <- runActions k l actions
  case filledOrders of
    Left l -> return $ Left l
    Right f -> processFills f
  where
    processFills f = case f of
      [] -> return $ Right (initState, simState)
      fills -> case sim levelState event simState of
        (_, _, True) -> return $ Left Success
        (newSimState, actions, False) -> applyActions k l levelState sim newSimState actions
        where
          levelState = updatePosition initState fills
          event = OrderFilled fills


executeLoop :: StockfighterApiKey -> Level -> MVar UpdateEvent -> LevelState -> Simulation a -> a -> IO LevelResult
executeLoop k l mv ls s init = do
  ue <- takeMVar mv
  case updateSim ue of
    Right (_, (_, _, True)) -> return Success
    Right (newLevelState, (newState, actions, False)) -> do
      actionsApplied <- actionApplier newLevelState s newState actions
      case actionsApplied of
        Left lr -> return lr
        Right (ls, ss) -> executeLoop k l mv ls s ss
    Left e -> return e
  where
    actionApplier = applyActions k l

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

  executeLoop k l comm defaultLevelState s a

runLevel' :: StockfighterApiKey -> InstanceId -> Simulation a -> a -> IO LevelResult
runLevel' k instanceId simFn a = do
  levelOrError <- restartLevel k instanceId
  case levelOrError of
    Right level -> simLevel k level simFn a
    Left jsonError -> return $ JsonError jsonError

runLevel :: StockfighterApiKey -> T.Text -> Simulation a -> a -> IO LevelResult
runLevel key levelName simulation a = do
  levelOrError <- startLevel key levelName
  case levelOrError of
    Right level -> runLevel' key (levelInstanceId level) simulation a
    Left jsonError -> return $ JsonError jsonError


marketOrder :: Integer -> Order
marketOrder qty =
  Order { orderQty = qty
        , orderPrice = StockPrice 0 -- Ignored for market order
        , orderType = Market
        , orderDir = Buy
        , orderId = Nothing }

level1 :: Simulation ()
level1 (LevelState {position}) _ _ =
  if remainingShares > 0 then
    ((), [PlaceOrder $  marketOrder remainingShares], False)
  else
    ((), [], True)
  where
    nShares = shares position
    remainingShares =  100 - nShares

apiKey :: T.Text
apiKey = "xxx"

-- To run the actual level:
-- runLevel (StockfighterApiKey apiKey) "first_steps" level1 ()
