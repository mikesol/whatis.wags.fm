module Util where

import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML.Properties as HP
import Lib (PlayingState(..))
import Types (RW)

rfToRW :: Ref.Ref ~> RW
rfToRW = { read: _, write: _ }
  <$> Ref.read
  <*> flip Ref.write

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

classesS :: forall r p. String -> HP.IProp (class :: String | r) p
classesS = classes <<< String.split (String.Pattern " ")


killNoMatterWhat :: Ref.Ref PlayingState -> Effect Unit -> Aff Unit
killNoMatterWhat pst doThing = do
  playingState <- H.liftEffect $ Ref.read pst
  case playingState of
    Stopped -> pure unit
    Playing _ -> H.liftEffect doThing
    Loading _ -> do
      delay (Milliseconds 100.0)
      killNoMatterWhat pst doThing
