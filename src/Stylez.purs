module Stylez where

import Prelude

import CSS (CSS, backgroundColor, bold, border, borderRadius, color, cursor, display, fontSize, fontWeight, inlineBlock, noneTextDecoration, paddingBottom, paddingLeft, paddingRight, paddingTop, px, rgb, solid, textDecoration, weight)
import CSS.Cursor (pointer)
import CSS.TextAlign (center, textAlign)

bttn :: CSS
bttn = do
  backgroundColor (rgb 255 255 255)
  border solid (0.0 # px) (rgb 255 255 255)
  let br = 10.0 # px
  borderRadius br br br br
  fontWeight (weight 600.0)
  cursor pointer
  color (rgb 0 0 0)
  let tb = 15.0 # px
  paddingTop tb
  paddingBottom tb
  let lr = 15.0 # px
  paddingLeft lr
  paddingRight lr
  textAlign center
  textDecoration noneTextDecoration
  display inlineBlock
  fontSize (16.0 # px)
