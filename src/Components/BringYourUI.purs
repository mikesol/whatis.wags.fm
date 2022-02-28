module Components.BringYourUI where

import Prelude

import CSS (column, cursor, display, flex, flexDirection, flexGrow, justifyContent, row, spaceBetween)
import CSS.Cursor (pointer)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import Effect.Ref as Ref
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events (onClick, onInput)
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Lib (initializeWags)
import Nonbili.DOM (innerText)
import SineQuaNon (SineQuaNon)
import Stylez (codeStyle)
import Util (classes, killNoMatterWhat, rfToRW)
import WAGS.Lib.Tidal.Types (ExternalControl)
import WAGS.Lib.Tidal.Util (r2b)
import Web.Event.Internal.Types (Event)

bringYourOwnUIExample :: String
bringYourOwnUIExample =
  """module BringYourOwnUI where

import Prelude

import WAGS.Create.Optionals (bandpass, gain, highpass, ref, sinOsc, triangleOsc)
import Data.Vec as V
import Data.Typelevel.Num.Reps (d0, d1)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Tidal.Types (AFuture)
import WAGS.Graph.Parameter (ff)
import WAGS.Graph.Paramable (paramize)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Tidal (addEffect, make, s)

f = ff 0.03

wag :: AFuture
wag =
  make 8.0
    { earth:
        map
          ( addEffect
              \{ clockTime, externalControl: { floats } } ->
                let
                  fAdd = V.index floats d0
                  freqAdd = V.index floats d1
                  fund = (256.0 / 4.0) + fAdd
                in
                  fx $ goodbye $ gain 1.0
                    { ipt: hello
                    , mxr: gain 1.0
                        { bp0: gain 0.2 $ bandpass
                            { freq: f $ paramize $ 2000.0 + freqAdd + lfo
                                { phase: 0.0
                                , freq: 0.3
                                , amp: 1500.0
                                }
                                clockTime
                            , q: 40.0
                            }
                            { oscs: ref }
                        , bp1: gain 0.2 $ highpass
                            { freq: f $ paramize $ 2300.0 + freqAdd + lfo
                                { phase: 0.0
                                , freq: 0.1
                                , amp: 1400.0
                                }
                                clockTime
                            , q: 30.0
                            }
                            { oscs: ref }
                        , muted: gain 0.0
                            { oscs: gain 1.0
                                { osc0: triangleOsc (f $ paramize $ fund)
                                , osc1: sinOsc (f $ paramize $ fund * 2.0 + 10.0)
                                }
                            }
                        }
                    }
          ) $ s $ "tink:1 tink:2 tink:3 tink:4"
    }
"""

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
  | RangeVal Int Number
  | ThisIsPush (Boolean -> Effect Unit)

component
  :: forall query input output m
   . MonadEffect m
  => SineQuaNon
  -> Ref.Ref ExternalControl
  -> H.Component query input output m

component i ctrl =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction i ctrl
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

foreign import unsafeParseEvent :: Event -> Number

propagateInput :: Int -> Event -> Action
propagateInput i = RangeVal i <<< unsafeParseEvent

render :: forall m. State -> H.ComponentHTML Action () m
render { audioUIState } =
  HH.div_
    [ HH.h3_ [ HH.text "bring your own UI" ]
    , HH.small_ [ HH.text "wags.fm supports thousands of configurable skins for visual music editing. press play & try the sliders below!" ]
    , HH.pre_
        [ HH.code
            (codeStyle rlabel DoPush)
            [ HH.text bringYourOwnUIExample ]
        ]
    , HH.div
        [ CSS.style do
            display flex
            flexDirection column
        ]
        [ HH.div [ CSS.style do flexGrow 1.0 ] []
        , HH.div
            [ CSS.style do
                display flex
                flexDirection row
                justifyContent spaceBetween
            ]
            [ HH.div [] []
            , HH.input [ HP.value "25", HP.min 0.0, HP.max 100.0, HP.type_ InputRange, onInput $ propagateInput 0 ]
            , HH.input [ HP.value "75", HP.min 0.0, HP.max 100.0, HP.type_ InputRange, onInput $ propagateInput 1 ]
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
            , HH.div [] []
            ]
        , HH.div [ CSS.style do flexGrow 1.0 ] []
        ]
    ]

handleAction :: forall output m. MonadEffect m => SineQuaNon -> Ref.Ref ExternalControl -> Action -> H.HalogenM State Action () output m Unit
handleAction
  { name
  , setAlert
  , modules
  , bufferCache
  , registerSlideChange
  , unregisterSlideChange
  , prevCycle
  , playingState
  }
  ctrl = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect $ HS.create
    subscription <- H.subscribe emitter
    elt <- H.getHTMLElementRef rlabel
    for_ elt \e -> do
      let
        buttonEffect = initializeWags
          (r2b ctrl)
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
  RangeVal i n -> do
    Log.info $ show n
    case i of
      0 -> H.liftEffect $ Ref.modify_ (\s -> s { floats = V.updateAt d0 n s.floats }) ctrl
      _ -> H.liftEffect $ Ref.modify_ (\s -> s { floats = V.updateAt d1 (n * 10.0) s.floats }) ctrl