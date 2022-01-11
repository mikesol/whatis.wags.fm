module Components.Starter where

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
import Halogen.HTML.Events (onClick, onKeyDown)
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Lib (initializeWags)
import Nonbili.DOM (innerText)
import SineQuaNon (SineQuaNon)
import Util (classes, killNoMatterWhat, rfToRW)
import WAGS.Lib.Tidal.Types (emptyCtrl)

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
    , render: render
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
    [ HH.h2_ [ HH.text "here's an example" ]
    , HH.small_ [ HH.text "(press play below)" ]
    , HH.pre_
        [ HH.code
            [ HH.attr (HH.AttrName "contenteditable") "true"
            , HP.ref rlabel
            , HH.attr (HH.AttrName "data-trim") "true"
            , HH.attr (HH.AttrName "data-noescape") "true"
            , onKeyDown (const DoPush)
            ]
            [ HH.text "future bd hh chin*4" ]
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
