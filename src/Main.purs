module Main where

import Prelude

import Components.BringYourUI as BringYourUI
import Components.ButtonsGalore as ButtonsGalore
import Components.FullDAW as FullDAW
import Components.NowTryChange as NowTryChange
import Components.AmazingWork as AmazingWork
import Components.Starter as Starter
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
import WAGS.Lib.Tidal.Types (emptyCtrl)

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
    HA.selectElement (wrap "#starter") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "starter"
          }
        -- Log.info "running ui"
        runUI (Starter.component sn) unit e
    HA.selectElement (wrap "#nowTryChange") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "nowTryChange"
          }
        -- Log.info "running ui"
        runUI (NowTryChange.component sn) unit e
    HA.selectElement (wrap "#buttonsGalore") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "buttonsGalore"
          }
        -- Log.info "running ui"
        runUI (ButtonsGalore.component sn) unit e
    HA.selectElement (wrap "#fullDAW") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "fullDAW"
          }
        -- Log.info "running ui"
        runUI (FullDAW.component sn) unit e
    HA.selectElement (wrap "#bringYourUI") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "bringYourUI"
          }
        -- Log.info "running ui"
        r <- liftEffect $ Ref.new emptyCtrl
        runUI (BringYourUI.component sn r) unit e
    HA.selectElement (wrap "#amazingWork") >>=
      flip for_ \e -> do
        sn <- liftEffect $ sqn
          { name: "amazingWork"
          }
        -- Log.info "running ui"
        runUI (AmazingWork.component sn) unit e
