module Main
  ( PersonOptions
  , Velocity
  , main
  ) where

import Prelude
import Canvas.Utils (cleanBody, createCanvasElement)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Graphics.Canvas as Canvas
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
  = { x :: Number, y :: Number, vel :: Velocity }
type Velocity
  = { x :: Number, y :: Number }

isOutOfXBound :: Number -> Boolean
isOutOfXBound x =
  let
    leftBoundary = x - personRadius
    rightBoundary = x + personRadius
  in
    leftBoundary <= 0.0 || rightBoundary >= canvasWidth

isOutOfYBound :: Number -> Boolean
isOutOfYBound y =
  let
    leftBoundary = y - personRadius
    rightBoundary = y + personRadius
  in
    leftBoundary <= 0.0 || rightBoundary >= canvasWidth

draw :: Canvas.Context2D -> PersonOptions -> Effect Unit
draw ctx options = do
  _ <-
    Canvas.clearRect ctx
      { x: 0.0
      , y: 0.0
      , width: canvasWidth
      , height: canvasHeight
      }
  _ <- Canvas.save ctx
  _ <- Canvas.setFillStyle ctx sanePersonColor
  _ <- Canvas.translate ctx { translateX: options.x, translateY: options.y }
  _ <- Canvas.beginPath ctx
  _ <-
    Canvas.arc ctx
      { x: 0.0
      , y: 0.0
      , radius: personRadius
      , start: 0.0
      , end: Math.tau
      }
  _ <- Canvas.fill ctx
  _ <- Canvas.stroke ctx
  _ <- Canvas.restore ctx
  win <- window
  let
    newY = options.y + options.vel.y
    newX = options.x + options.vel.x
    newVelX = options.vel.x * if (isOutOfXBound newX) then -1.0 else 1.0
    newVelY = options.vel.y * if (isOutOfYBound newY) then -1.0 else 1.0
    newOptions = { x: newX, y: newY, vel: { x: newVelX, y: newVelY } }
  _ <- requestAnimationFrame (draw ctx newOptions) win
  pure unit

randomPersonOption :: Effect PersonOptions
randomPersonOption = do
  velX <- randomRange (-2.0) 2.0
  velY <- randomRange (-2.0) 2.0
  centerX <- randomRange 0.0 canvasWidth
  centerY <- randomRange 0.0 canvasHeight
  pure { x: centerX, y: centerY, vel: { x: velX, y: velY } }

main :: Effect Unit
main = do
  _ <- cleanBody
  canvas <- createCanvasElement canvasHeightInt canvasWidthInt
  ctx <- Canvas.getContext2D canvas
  personOption <- randomPersonOption
  draw ctx personOption
