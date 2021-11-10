module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (either)
import Control.Monad.Except (runExcept)
import Web.DOM.Element (Element, toNode)
import Web.HTML (window, HTMLDocument, HTMLElement)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement, setWidth, setHeight)
import Web.DOM.HTMLCollection (item, toArray)
import Foreign (Foreign, F, unsafeToForeign, unsafeReadTagged)
import Web.DOM.Node (Node, appendChild, removeChild, parentNode)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.Window (document)
import Effect.Console (log)
import Data.Int (toNumber)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (Document, createElement, getElementsByTagName)
import Graphics.Canvas (CanvasElement, getContext2D, setFillStyle, fillRect)
import Data.Traversable (for)

htmlDocumentToDocument :: HTMLDocument -> Document
htmlDocumentToDocument = unsafeCoerce

readHTMLCanvasElement :: Foreign -> F HTMLCanvasElement
readHTMLCanvasElement = unsafeReadTagged "HTMLCanvasElement"

htmlCanvasElementToHTMLElement :: HTMLCanvasElement -> HTMLElement
htmlCanvasElementToHTMLElement = unsafeCoerce

htmlElementToElement :: HTMLElement -> Element
htmlElementToElement = unsafeCoerce

toCustomCanvasElement :: HTMLCanvasElement -> CanvasElement
toCustomCanvasElement = unsafeCoerce

toCanvasElement :: Element -> Maybe HTMLCanvasElement
toCanvasElement = either (const Nothing) Just <<< runExcept <<< readHTMLCanvasElement <<< unsafeToForeign

htmlDocument :: Effect Document
htmlDocument = htmlDocumentToDocument <$> (document =<< window)

removeAllNodes :: Array Node -> Effect (Array Unit)
removeAllNodes nodes =
  for nodes \node -> do
    maybeParentNode <- parentNode node
    removeChild node (unsafePartial fromJust maybeParentNode)

cleanBody :: Effect (Array Unit)
cleanBody = do
  doc <- htmlDocument
  let canvasEls = toArray =<< (getElementsByTagName "canvas" doc)
  canvasNodes <- map (\els -> map toNode els) canvasEls
  removeAllNodes canvasNodes

-- adapted from https://github.com/chexxor/purescript-pong-example/blob/master/src/Main.purs
createCanvasElement :: Int -> Int -> Effect CanvasElement
createCanvasElement canvasHeight canvasWidth = do
  doc <- htmlDocument
  bodyes <- getElementsByTagName "body" doc
  bodyEl <- item 0 bodyes
  let body = unsafePartial fromJust bodyEl
  canvasEl <- toCanvasElement <$> (createElement "canvas" doc)
  let canvasEl' = unsafePartial fromJust canvasEl
  setWidth canvasWidth canvasEl'
  setHeight canvasHeight canvasEl'
  _ <-
    appendChild
      (toNode $ htmlElementToElement $ htmlCanvasElementToHTMLElement canvasEl')
      (toNode body)
  pure $ unsafePartial fromJust $ map toCustomCanvasElement canvasEl

main :: Effect Unit
main = do
  _ <- cleanBody
  canvas <- createCanvasElement 500 400
  context2d <- getContext2D canvas
  _ <- setFillStyle context2d "black"
  _ <- fillRect context2d { height: toNumber 90, width: toNumber 20, x: toNumber 80, y: toNumber 90 }
  log ("canvas created")
