module SineQuaNon where
import Prelude

import Data.Variant.Maybe as VM
import Effect (Effect)
import Effect.Ref as Ref
import JIT.EvalSources (Modules)
import Lib (PlayingState(..))
import WAGS.Lib.Tidal.Tidal as C
import WAGS.Lib.Tidal.Tidal as T
import WAGS.Lib.Tidal.Types (SampleCache)
import WAGS.Lib.Tidal.Types as TT

sineQuaNon
  :: { name :: String
     , setAlert :: String -> Effect Unit
     , bufferCache :: Ref.Ref SampleCache
     , modules :: Ref.Ref Modules
     , registerSlideChange :: String -> Effect Unit -> Effect Unit
     , unregisterSlideChange :: String -> Effect Unit
     }
  -> Effect SineQuaNon
sineQuaNon { name, setAlert, bufferCache, modules, registerSlideChange, unregisterSlideChange } = ado
  playingState <- Ref.new Stopped
  prevCycle <- Ref.new C.intentionalSilenceForInternalUseOnly_
  in
    { name
    , setAlert
    , bufferCache
    , modules
    , registerSlideChange
    , unregisterSlideChange
    , prevCycle
    , playingState
    }

type SineQuaNon =
  { name :: String
  , setAlert :: String -> Effect Unit
  , bufferCache :: Ref.Ref SampleCache
  , modules :: Ref.Ref Modules
  , registerSlideChange :: String -> Effect Unit -> Effect Unit
  , unregisterSlideChange :: String -> Effect Unit
  , prevCycle :: Ref.Ref (T.Cycle (VM.Maybe (TT.Note Unit)))
  , playingState :: Ref.Ref PlayingState
  }