module Components.FullDAW where

import Prelude

import CSS (cursor)
import CSS.Cursor (pointer)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events (onClick)
import Halogen.Subscription as HS
import Lib (initializeWags)
import Nonbili.DOM (innerText)
import SineQuaNon (SineQuaNon)
import Stylez (codeStyle)
import Util (classes, killNoMatterWhat, rfToRW)
import WAGS.Lib.Tidal.Types (emptyCtrl)

fullDAWExample :: String
fullDAWExample = """module FullDAW where

import Prelude

import Data.Lens (set)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Math ((%))
import WAGS.Create.Optionals (highpass, pan)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Tidal (changeRate, changeVolume, lvt, make, onTag, parse, s)

m2 = 4.0 * 1.0 * 60.0/111.0 :: Number

wag :: AFuture
wag =
  make (m2 * 2.0)
    { earth: s $ map (changeRate (\{ normalizedLittleCycleTime: t } -> 1.0 + t*0.1 )) $ parse "tink:1;t0 tink:2;t1 tink:3;t2 tink:0;t3 tink:4;t4 tink:2;t5 tink:3;t6 tink:1;t7 tink:2;t8 tink:0;t9 tink:3;t10 "
    , wind: map
        ( set lvt
            (lcmap unwrap \{ clockTime } -> let mody = clockTime % (m2 * 2.0) in fx
                ( goodbye $ highpass (200.0 + mody * 100.0) hello
                )
            )
        ) $ s $ onTag "ph" (changeRate \{ normalizedSampleTime: t } -> min 1.2 (1.0 + t*0.3) )
           $ onTag "print" (changeVolume \{ normalizedSampleTime: _ } -> 0.2 )
           $ onTag "pk" (changeRate \{ normalizedSampleTime: t } -> 0.7 - t*0.2 )
           $ onTag "kt" (changeRate \{ normalizedSampleTime: t } -> min 1.0 (0.6 + t*0.8) ) $ parse "[psr:3 [~ chin*4]] [~ ~ psr:3;ph ~ psr:3;ph ~ ] , [~ ~ ~ <psr:1;print kurt:0;print> ] kurt:5;kt , ~ ~ pluck:1;pk ~ ~ ~ ~ ~ "
    , fire: map
        ( set lvt
            (lcmap unwrap \{ clockTime } -> fx
                ( goodbye $ pan (lfo { phase: 0.0, amp: 1.0, freq: 0.2 } clockTime + 0.0) { myhp: highpass (lfo { phase: 0.0, amp: 2000.0, freq: 0.4 } clockTime + 2000.0) hello }
                )
            )
        ) $ s "~ ~ ~ ~ ~ ~ speechless:2 ~"
    }"""

type State =
  { subscription :: Maybe H.SubscriptionId
  , audioUIState :: AudioUIDislay
  , buttonEffect :: Effect Unit
  , push :: Boolean -> Effect Unit
  }

rlabel :: RefLabel
rlabel = RefLabel "textarea"

data AudioUIDislay = AUPlaying | AULoading | AUStopped

data Action
  = Initialize
  | Finalize
  | StopAudio
  | LoadAudio
  | AudioLoaded
  | PressButton
  | DoPush
  | ThisIsPush (Boolean -> Effect Unit)

component
  :: forall query input output m
   . MonadEffect m
  => SineQuaNon
  -> H.Component query input output m

component i =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction i
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { subscription: Nothing
  , audioUIState: AUStopped
  , buttonEffect: mempty
  , push: mempty
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { audioUIState } =
  HH.div_
    [ HH.h3_ [ HH.text "wags.fm is a full-featured DAW"  ]
    , HH.small_ [ HH.text "its unique text-based format allows musicians to innovate using the power of code." ]
    , HH.pre_
        [ HH.code
            (codeStyle rlabel DoPush)
            [ HH.text fullDAWExample ]
        ]
    , HH.i
        [ classes
            ( [ "fas", "fa-lg" ] <> case audioUIState of
                AUPlaying -> [ "fa-stop-circle" ]
                AULoading -> [ "fa-spinner", "fa-spin" ]
                AUStopped -> [ "fa-play-circle" ]
            )
        , CSS.style do
            cursor pointer
        , onClick (const PressButton)
        ]
        []
    ]

handleAction :: forall output m. MonadEffect m => SineQuaNon -> Action -> H.HalogenM State Action () output m Unit
handleAction
  { name
  , setAlert
  , modules
  , bufferCache
  , registerSlideChange
  , unregisterSlideChange
  , prevCycle
  , playingState
  } = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect $ HS.create
    subscription <- H.subscribe emitter
    elt <- H.getHTMLElementRef rlabel
    for_ elt \e -> do
      let
        buttonEffect = initializeWags
          (pure emptyCtrl)
          (rfToRW prevCycle)
          (rfToRW playingState)
          (rfToRW bufferCache)
          (rfToRW modules)
          (innerText e)
          setAlert
          mempty
          (HS.notify listener LoadAudio)
          (HS.notify listener StopAudio)
          (HS.notify listener AudioLoaded)
          (HS.notify listener <<< ThisIsPush)
      H.modify_ _
        { subscription = Just subscription
        , buttonEffect = buttonEffect
        }
      H.liftEffect
        $ registerSlideChange name
        $ launchAff_
        $ killNoMatterWhat playingState buttonEffect

  Finalize -> do
    H.liftEffect $ unregisterSlideChange name
    H.gets _.subscription >>= flip for_ H.unsubscribe
  StopAudio -> do
    H.modify_ _ { audioUIState = AUStopped }
  LoadAudio -> do
    H.modify_ _ { audioUIState = AULoading }
  AudioLoaded -> do
    H.modify_ _ { audioUIState = AUPlaying }
  ThisIsPush push -> do
    H.modify_ _ { push = push }
  DoPush -> do
    H.gets _.push >>= H.liftEffect <<< (#) false
  PressButton -> do
    H.gets _.buttonEffect >>= H.liftEffect
