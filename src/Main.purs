module Main
  ( PersonOptions
  , main
  ) where

import Prelude
import Canvas.Utils (cleanBody, createCanvasElement)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Graphics.Canvas (CanvasElement, Context2D, arc, beginPath, clearRect, fill, fillPath, getContext2D, restore, save, setFillStyle, stroke, translate)
import Math as Math
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

canvasHeightInt :: Int
canvasHeightInt = 500

canvasHeight :: Number
canvasHeight = toNumber canvasHeightInt

canvasWidthInt :: Int
canvasWidthInt = 400

canvasWidth :: Number
canvasWidth = toNumber canvasWidthInt

personRadiusInt :: Int
personRadiusInt = 10

personRadius :: Number
personRadius = toNumber personRadiusInt

sanePersonColor :: String
sanePersonColor = "#FFF"

type PersonOptions
  = { x :: Number, y :: Number, vel :: Number }

draw :: Context2D -> PersonOptions -> Effect Unit
draw ctx options = do
  _ <-
    clearRect ctx
      { x: 0.0
      , y: 0.0
      , width: canvasWidth
      , height: canvasHeight
      }
  _ <- save ctx
  _ <- setFillStyle ctx sanePersonColor
  _ <- translate ctx { translateX: options.x, translateY: options.y }
  _ <- beginPath ctx
  _ <-
    arc ctx
      { x: 0.0
      , y: 0.0
      , radius: personRadius
      , start: 0.0
      , end: Math.tau
      }
  _ <- fill ctx
  _ <- stroke ctx
  _ <- restore ctx
  win <- window
  _ <-
    requestAnimationFrame
      ( draw ctx
          options
            { x = options.x + options.vel
            , y = options.y + options.vel
            }
      )
      win
  pure unit

main :: Effect Unit
main = do
  _ <- cleanBody
  canvas <- createCanvasElement canvasHeightInt canvasWidthInt
  ctx <- getContext2D canvas
  vel <- randomRange (-1.0) 1.0
  draw ctx
    { x: canvasWidth / 2.0
    , y: canvasHeight / 2.0
    , vel: vel
    }
