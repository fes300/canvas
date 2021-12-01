module Main
  ( PersonOptions
  , Velocity
  , main
  ) where

import Prelude
import Canvas.Utils (cleanBody, createCanvasElement)
import Data.Int (toNumber)
import Data.List (List, range)
import Data.Traversable (traverse)
import Effect (Effect)
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

drawPerson :: Canvas.Context2D -> PersonOptions -> Effect Unit
drawPerson ctx options = do
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
  pure unit

updatePeople :: List PersonOptions -> List PersonOptions
updatePeople oldPeople = map updatePerson oldPeople
  where
  updatePerson :: PersonOptions -> PersonOptions
  updatePerson op =
    let
      newY = op.y + op.vel.y
      newX = op.x + op.vel.x
      newVelX = op.vel.x * if (isOutOfXBound newX) then -1.0 else 1.0
      newVelY = op.vel.y * if (isOutOfYBound newY) then -1.0 else 1.0
    in
      { x: newX, y: newY, vel: { x: newVelX, y: newVelY } }

draw :: Canvas.Context2D -> List PersonOptions -> Effect Unit
draw ctx people = do
  _ <-
    Canvas.clearRect ctx
      { x: 0.0
      , y: 0.0
      , width: canvasWidth
      , height: canvasHeight
      }
  _ <- traverse (\o -> drawPerson ctx o) people
  win <- window
  let newPeople = updatePeople people
  _ <- requestAnimationFrame (draw ctx newPeople) win
  pure unit

randomPersonOption :: Effect PersonOptions
randomPersonOption = do
  velX <- randomRange (-2.0) 2.0
  velY <- randomRange (-2.0) 2.0
  centerX <- randomRange personRadius (canvasWidth - personRadius)
  centerY <- randomRange personRadius (canvasHeight - personRadius)
  pure { x: centerX, y: centerY, vel: { x: velX, y: velY } }

main :: Effect Unit
main = do
  _ <- cleanBody
  canvas <- createCanvasElement canvasHeightInt canvasWidthInt
  ctx <- Canvas.getContext2D canvas
  options <- traverse (\_ -> randomPersonOption) (range 1 10)
  draw ctx options
