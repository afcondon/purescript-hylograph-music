-- | Anscombe's Quartet - Timer
-- |
-- | Clean timer abstraction for Halogen subscriptions.
-- | Uses native setInterval/setTimeout which properly yield to the event loop.
module Anscombe.Timer
  ( IntervalId
  , setInterval
  , clearInterval
  , setTimeout
  ) where

import Prelude

import Effect (Effect)

-- | Opaque handle for cancelling an interval
foreign import data IntervalId :: Type

-- | Set up a repeating callback. Returns handle for cancellation.
foreign import setInterval :: Number -> Effect Unit -> Effect IntervalId

-- | Cancel a repeating interval
foreign import clearInterval :: IntervalId -> Effect Unit

-- | Set up a one-shot callback after a delay (milliseconds)
foreign import setTimeout :: Number -> Effect Unit -> Effect Unit
