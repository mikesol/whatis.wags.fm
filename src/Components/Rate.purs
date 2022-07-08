module Components.Rate where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Now (now)
import FRP.Behavior (Behavior, sampleBy)
import FRP.Behavior.Time (instant)
import FRP.Event (Event, mapAccum)
import Safe.Coerce (coerce)

type RateInfo =
  { prevTime :: Maybe Number
  , time :: Number
  , prevBeats :: Maybe Number
  , beats :: Number
  , epochTime :: Milliseconds
  }

timeFromRate
  :: Behavior { rate :: Number }
  -> Event { real :: Number }
  -> Event RateInfo
timeFromRate clengthB afE = mapAccum
  ( \{ behaviors: { clength, epochTime }, acTime } { prevTime, prevBeats } -> do
      let prevAC = fromMaybe (0.0) prevTime
      let prevAJ = fromMaybe (0.0) prevBeats
      let gap = acTime - prevAC
      let adjGap = gap / (clength)
      let beats = ((adjGap) + (prevAJ))
      let retval = { prevTime, prevBeats, time: acTime, beats, epochTime }
      -- let ____ = spy "info" retval
      { prevTime: Just $ acTime, prevBeats: Just beats } /\ retval
  )
  ( sampleBy { behaviors: _, acTime: _ }
      ( { clength: _, epochTime: _ }
          <$> (_.rate <$> (clengthB))
          <*> (coerce <$> (unInstant <$> instant))
      )
      (_.real <$> afE)
  )
  { prevTime: Nothing, prevBeats: Nothing }
