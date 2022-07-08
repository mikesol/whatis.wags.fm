module Main where

import Prelude

import Components.Hello (hello)
import Components.HelloAudio (introRun)
import Data.Foldable (for_)
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (log)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

data Notfy

foreign import setAlert_ :: Notfy -> String -> Effect Unit

runMe d s c = do
  e <- getElementById s (toNonElementParentNode (toDocument d))
  for_ e (void <<< c)

main  :: Effect Unit
main = do
  w <- window
  d <- document w
  runMe d "hello-sdom" hello
  runMe d "hello-sdom-audio" introRun
  pure unit