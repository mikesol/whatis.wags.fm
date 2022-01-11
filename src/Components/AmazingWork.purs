module Components.AmazingWork where

import Prelude

import CSS (column, cursor, display, flex, flexDirection, flexGrow, justifyContent, px, row, spaceBetween, width)
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
import Halogen.HTML.Events (onClick, onKeyDown)
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Lib (initializeWags)
import Nonbili.DOM (innerText)
import SineQuaNon (SineQuaNon)
import Stylez (bttn)
import Util (classes, killNoMatterWhat, rfToRW)
import WAGS.Lib.Tidal.Types (emptyCtrl)

type State =
  { subscription :: Maybe H.SubscriptionId
  , audioUIState :: AudioUIDislay
  , buttonEffect :: Effect Unit
  , muzak :: String
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
  , muzak: "future bd hh chin*4"
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.h3_ [ HH.text "here's some amazing work" ]
    , HH.h3_ [ HH.text "created by our community" ]
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
            , HH.a [ CSS.style bttn, HP.href "https://yap.wags.fm/p/09-12-2021" ] [ HH.text "happy birthday" ]
            , HH.a [ CSS.style bttn, HP.href "https://yap.wags.fm/p/01-01-2022" ] [ HH.text "rauhaa, vain rauhaa" ]
            , HH.a [ CSS.style bttn, HP.href "https://yap.wags.fm/p/21-12-2021" ] [ HH.text "dirty beats" ]
            , HH.div [] []
            ]
        , HH.div [ CSS.style do flexGrow 1.0 ] []
        ]
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
