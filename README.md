# Stockfighter API Client

A Haskell API that can be used for the [Stockfighter](stockfighter.io)
game.

## TODO

Right now this is mostly a sketch of how the system would work. Things
that are on the TODO list to make this actually useful:

* Implement WebSocket API
* Implement Order Cancellation
* Use a [real logging framework](http://hackage.haskell.org/package/hslogger-1.2.10/docs/System-Log-Logger.html)
  instead of `Debug.Trace`

## Usage

The idea behind the library is that the user needs to implement a simple
pure function that describes the algorithm they wish to implement.

The signature of the algorithm is:

```haskell
type Simulation a = LevelState -> Event -> a -> (a, [Action], Bool)
```

Where `LevelState` contains info about the Order Book, recent quotes,
and your current position and `a` is some state that you want to keep
track of. Whenever an update occurs (order filled, canceled, order book
updated, new quote, etc) the function is called with that event info.

To respond to the event return your new custom state, a list of
`Action`, representing buys and sells, and a flag that indicates the
level is over.

## Example

The first level of Stockfighter is to buy 100 shares. The level looks
something like:

```haskell
-- We don't keep any custom state so we use ()
-- The only thing we care about is our current position
-- and we can ignore the event
level1 (LevelState {position}) _ _ =
  if remainingShares > 0 then
    ((), [PlaceOrder $  marketOrder remainingShares], False)
  else
    ((), [], True)
  where
    nShares = shares position
    remainingShares =  100 - nShares
```

We can execute this level via:

```haskell
apiKey :: T.Text
apiKey = "xxx"

runLevel (StockfighterApiKey apiKey) "first_steps" level1 ()
```

## Weirdness

The project layout is a bit haphazard due to this being my first Haskell
project and also dealing with record name issues (duplicate names not
being allowed).
