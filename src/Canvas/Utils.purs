module Canvas.Utils where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for)
import Effect (Effect)
import Foreign (Foreign, F, unsafeToForeign, unsafeReadTagged)
import Graphics.Canvas (CanvasElement)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, createElement, getElementsByTagName)
import Web.DOM.Element (Element, toNode)
import Web.DOM.HTMLCollection (item, toArray)
import Web.DOM.Node (Node, appendChild, removeChild, parentNode)
import Web.HTML (window, HTMLDocument, HTMLElement)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement, setWidth, setHeight)
import Web.HTML.Window (document)

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
