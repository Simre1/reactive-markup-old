module ReactiveMarkup.Elements.DrawingBoard where

import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents

import qualified Diagrams.Core.Types as D

data DrawingBoard (o :: [*]) deriving Typeable

data instance Element (DrawingBoard options) elems e = DrawingBoard (Options options e)

drawingBoard :: Options options e -> Markup '[DrawingBoard options] '[] e
drawingBoard = toMarkup . DrawingBoard

data DrawDiagram b deriving Typeable

data instance Element (DrawDiagram b) elems e = DrawDiagram (D.Diagram b)

data DrawDynamicDiagram b deriving Typeable

data instance Element (DrawDynamicDiagram b) elems e = DrawDynamicDiagram (Dynamic (D.Diagram b))

drawDiagram :: Typeable b => D.Diagram b -> Options '[DrawDiagram b] e
drawDiagram = makeOption . DrawDiagram

drawDynamicDiagram :: Typeable b => Dynamic (D.Diagram b) -> Options '[DrawDynamicDiagram b] e
drawDynamicDiagram = makeOption . DrawDynamicDiagram

data MouseClickWithPosition deriving Typeable

data instance Element (MouseClickWithPosition) elems e = MouseClickWithPosition ((Double, Double) -> e)

mouseClickWithPosition :: ((Double,Double) -> e) -> Options '[MouseClickWithPosition] e
mouseClickWithPosition = makeOption . MouseClickWithPosition

data AspectRatio deriving Typeable

data instance Element (AspectRatio) elems e = AspectRatio (Double)

aspectRatio :: (Double) -> Options '[AspectRatio] e
aspectRatio = makeOption . AspectRatio