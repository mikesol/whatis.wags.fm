module Components.ButtonsGalore where

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
  | LoFi
  | India
  | Jungle

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
  , muzak: jungle
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { audioUIState, muzak } =
  HH.div_
    [ HH.h3_ [ HH.text "use the buttons below to change the example" ]
    , HH.small_
        [ HH.text "or experiment with any sound listed "
        , HH.a [ HP.target "_blank", HP.href "https://github.com/mikesol/wagsi/blob/main/SOUNDS.md" ] [ HH.text "here." ], HH.text " it's all still editable!"
        ]
    , HH.pre_
        [ HH.code
            [ HH.attr (HH.AttrName "contenteditable") "true"
            , HP.ref rlabel
            , HH.attr (HH.AttrName "data-trim") "true"
            , HH.attr (HH.AttrName "data-noescape") "true"
            , onKeyDown (const DoPush)
            ]
            [ HH.text muzak ]
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
            , HH.button [ onClick (const India), CSS.style bttn ] [ HH.text "india" ]
            , HH.button [ onClick (const LoFi), CSS.style bttn ] [ HH.text "lo-fi" ]
            , HH.button [ onClick (const Jungle), CSS.style bttn ] [ HH.text "jungle" ]
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

handleAction :: forall output m. MonadEffect m => SineQuaNon -> Action -> H.HalogenM State Action () output m Unit
handleAction
  ipt@{ name
  , setAlert
  , modules
  , bufferCache
  , registerSlideChange
  , unregisterSlideChange
  , prevCycle
  , playingState
  } =
  let
    mdfy snd = do
      H.modify_ _ { muzak = snd }
      handleAction ipt DoPush
  in
    case _ of
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
      LoFi -> mdfy
        """[fatkick knuckle]
[chh1 [chh2 wtf]]
[fatkick chh1 chh2 chh1]
[[bttl,chh1] [purtty,blippy*3]]

# @duration 2.2
# @sample knuckle https://freesound.org/data/previews/5/5538_9151-hq.mp3
# @sample fatkick https://freesound.org/data/previews/344/344757_1676145-lq.mp3
# @sample chh1 https://freesound.org/data/previews/13/13246_36719-lq.mp3
# @sample chh2 https://freesound.org/data/previews/207/207905_19852-lq.mp3
# @sample wtf https://freesound.org/data/previews/193/193702_97763-lq.mp3
# @sample bttl https://freesound.org/data/previews/178/178659_717950-lq.mp3
# @sample blippy https://freesound.org/data/previews/336/336928_685248-lq.mp3
# @sample purtty https://freesound.org/data/previews/531/531510_7614679-lq.mp3
"""
      India -> mdfy
        """tabla:20 tabla:1 tabla:3 tabla:16
# @duration 3.0"""
      Jungle -> mdfy jungle

jungle :: String
jungle = """<
  [bd bd hh*2 [notes:6, chin*4]]
  [bd bd hh*2 [notes:7, chin*4]]
  [bd bd hh*2 [notes:8, chin*4]]
  [psr:2 hh hh hh*2]
  [bd bd hh*2 [notes:10, chin*4]]
  [bd bd hh*3 [notes:12, chin*4]]
  [bd bd hh*2 [notes:13, chin*4]]
  [bd bd hh*2 [notes:1, chin*4]]

  [[bd,hh:4] bd hh*2 [notes:6, chin*4] bd bd hh*2 [notes:7, chin*4]]
  [bd bd hh*2 [notes:8, chin*4] bd bd hh*2 [notes:9, chin*4]]
  [[bd,hh:5] bd hh*2 [notes:10, chin*4] bd bd hh*2 [notes:12, chin*4]]
  [bd bd hh*2 [notes:13, chin*4] bd bd hh*2 [notes:14, chin*4]]

# @duration 0.6
>"""
