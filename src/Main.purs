module Main where

import Prelude

import Components.FullDAW as FullDAW
import Data.Foldable (for_)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import JIT.EvalSources (freshModules)
import SineQuaNon (sineQuaNon)

data Notfy

foreign import setAlert_ :: Notfy -> String -> Effect Unit

main
  :: Effect
       { registerSlideChange :: String -> Effect Unit -> Effect Unit
       , unregisterSlideChange :: String -> Effect Unit
       , notfy :: Effect Notfy
       }
  -> Effect Unit
main fx = do
  -- Log.info "main called"
  { registerSlideChange
  , unregisterSlideChange
  , notfy
  } <- fx
  bufferCache <- Ref.new Object.empty
  modules <- freshModules >>= Ref.new
  HA.runHalogenAff do
    HA.awaitLoad
    notfy' <- liftEffect notfy
    let
      setAlert = setAlert_ notfy'
      sqn { name } = sineQuaNon
        { name
        , setAlert
        , bufferCache
        , modules
        , registerSlideChange
        , unregisterSlideChange
        }
    HA.selectElement (wrap "#allINeed") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "allINeed"
          }
        -- Log.info "running ui"
        runUI (FullDAW.component sn) unit e
