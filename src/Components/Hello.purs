module Components.Hello where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Listeners (click_, slider)
import Deku.Toplevel (runInBody)
import Deku.Toplevel (runInElement')
import Effect (Effect)
import FRP.Event (bang, fold)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Web.DOM (Element)

type UIEvents = V
  ( buttonClicked :: Unit
  , sliderMoved :: Number
  )

hello :: Element -> Effect (Effect Unit)
hello e = runInElement' e
  ( envy $ vbus (Proxy :: _ UIEvents) \push event -> do
      D.div_
        [ D.button
            ( oneOf
                [ bang $ D.Class := "button-1"
                , click_ (bang push.buttonClicked)
                ]
            )
            [ D.span (bang $ D.Style := "font-size: x-large;") [text_ "Click Me" ]]
        , D.div_
            [ text
                ( bang "Val: 0" <|>
                    ( append "Val: " <<< show
                        <$> fold
                          (const (add 1))
                          (bang unit <|> event.buttonClicked)
                          (-1)
                    )
                )
            ]
        , D.div_
            [ D.input
                (slider (bang push.sliderMoved))
                []
            , D.div_
                [ text
                    ( bang "Val: 50" <|>
                        ( append "Val: " <<< show
                            <$> event.sliderMoved
                        )
                    )
                ]
            ]
        ]
  )
