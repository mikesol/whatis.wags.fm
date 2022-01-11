module Types where

import Prelude

import Effect (Effect)

type RW x = { read :: Effect x, write :: x -> Effect Unit }
