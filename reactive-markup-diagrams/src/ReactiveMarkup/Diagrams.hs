module ReactiveMarkup.Diagrams where

import qualified Diagrams.Backend.Cairo as BC (Cairo) 
import qualified Diagrams.Backend.Cairo.Ptr as BC (renderPtr)
import qualified GI.Cairo as GIC
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Internal as C (runRender, Cairo(..))
import Diagrams.Core (Diagram)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.IORef

import ReactiveMarkup.Runners.Gtk
import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents

import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Marshal.Alloc (free)


import qualified Diagrams.Core.Types as D

data DrawingArea (o :: [*]) deriving Typeable

data instance Element (DrawingArea options) elems e = DrawingArea (Options options e)

drawingArea :: Options options e -> Markup '[DrawingArea options] '[] e
drawingArea = toMarkup . DrawingArea

data DrawDrawingArea b deriving Typeable

data instance Element (DrawDrawingArea b) elems e = DrawDrawingArea (D.Diagram b)

data DrawDynamicDrawingArea b deriving Typeable

data instance Element (DrawDynamicDrawingArea b) elems e = DrawDynamicDrawingArea (Dynamic (D.Diagram b))

drawDrawingArea :: Typeable b => D.Diagram b -> Options '[DrawDrawingArea b] e
drawDrawingArea = makeOption . DrawDrawingArea

drawDynamicDrawingArea :: Typeable b => Dynamic (D.Diagram b) -> Options '[DrawDynamicDrawingArea b] e
drawDynamicDrawingArea = makeOption . DrawDynamicDrawingArea

data MouseClickWithPosition deriving Typeable

data instance Element (MouseClickWithPosition) elems e = MouseClickWithPosition ((Double, Double) -> e)

mouseClickWithPosition :: ((Double,Double) -> e) -> Options '[MouseClickWithPosition] e
mouseClickWithPosition = makeOption . MouseClickWithPosition

data AspectRatio deriving Typeable

data instance Element (AspectRatio) elems e = AspectRatio (Double)

aspectRatio :: (Double) -> Options '[AspectRatio] e
aspectRatio = makeOption . AspectRatio


runDrawingArea :: Runner '[DrawingArea '[DrawDrawingArea BC.Cairo, DrawDynamicDrawingArea BC.Cairo, MouseClickWithPosition, AspectRatio]] (IO ()) (GtkM Gtk.Widget)
runDrawingArea = eventRun $ \(DrawingArea (Options options)) handleEvent -> do
  drawingArea <- Gtk.new Gtk.DrawingArea [#expand Gtk.:= True]
  Gtk.widgetAddEvents drawingArea [Gdk.EventMaskAllEventsMask]
  diagramRef <- liftIO $ newIORef Nothing
  aspectRatio <- liftIO $ newIORef Nothing
  traverse (runMarkup (optionsRunner aspectRatio diagramRef drawingArea) handleEvent) options
  maybeDrawingArea <- liftIO $ readIORef diagramRef
  maybeAspectRatio <- liftIO $ readIORef aspectRatio
  liftIO $ case maybeDrawingArea of
    Nothing -> pure ()
    Just dynamicDrawingArea -> do
      (resizeDynamic, triggerResize) <- newDynamic (0,0)
      imagePtr <- newIORef (nullPtr)
      unregister1 <- reactimate (toEvent $ (,) <$> dynamicDrawingArea <*> resizeDynamic) $ simpleEventHandler $ \(diagram, (w,h)) -> do
        oldPtr <- readIORef imagePtr
        BC.renderPtr w h (C.FormatARGB32) diagram >>= atomicWriteIORef imagePtr
        free oldPtr
      Gtk.onWidgetSizeAllocate drawingArea $ \rect -> do
        w <- Gdk.getRectangleWidth rect
        h <- Gdk.getRectangleHeight rect
        triggerEvent triggerResize (fromEnum w, fromEnum h)
      Gtk.onWidgetDraw drawingArea $ \context -> do
        (width, height) <- current $ toBehavior resizeDynamic
        ptr <- readIORef imagePtr
        C.withImageSurfaceForData (castPtr ptr) C.FormatARGB32 width height (C.formatStrideForWidth C.FormatARGB32 width) $ \surface -> do
          runRenderWithContext context $ do
            C.setSourceSurface surface 0 0
            C.newPath
            C.rectangle 0 0 (fromIntegral width) (fromIntegral height)
            C.paint
        pure True
      unregister2 <- reactimate (toEvent dynamicDrawingArea) $ simpleEventHandler $ \_ -> Gtk.widgetQueueDraw drawingArea
      Gtk.on drawingArea #destroy (liftES $ unregister1 *> unregister2)
      pure ()
  case maybeAspectRatio of
    Nothing -> Gtk.toWidget drawingArea
    (Just r) -> do
      frame <- Gtk.new Gtk.AspectFrame [#expand Gtk.:= True]
      Gtk.aspectFrameSet frame 0.5 0.5 (realToFrac r) False
      #add frame drawingArea
      Gtk.toWidget frame
  where
    optionsRunner :: IORef (Maybe Double) -> IORef (Maybe (Dynamic (Diagram BC.Cairo))) -> Gtk.DrawingArea -> Runner [DrawDrawingArea BC.Cairo, DrawDynamicDrawingArea BC.Cairo, MouseClickWithPosition, AspectRatio] (IO ()) (GtkM ())
    optionsRunner aspectRef diagramRef drawingArea = 
          simpleRun (\(DrawDrawingArea d) -> liftIO $ writeIORef diagramRef $ Just $ pure d)
      |-> simpleRun (\(DrawDynamicDrawingArea d) -> liftIO $ writeIORef diagramRef $ Just d)
      |-> eventRun (\(MouseClickWithPosition f) handleEvent -> do
            Gtk.onWidgetButtonPressEvent drawingArea $ \eventButton -> do
              mouseButton <- Gdk.getEventButtonButton eventButton
              when (mouseButton == 1) $ do
                  width <- fromIntegral <$> Gtk.widgetGetAllocatedWidth drawingArea
                  height <- fromIntegral <$> Gtk.widgetGetAllocatedHeight drawingArea
                  eventX <- floor <$> Gdk.getEventButtonX eventButton
                  eventY <- floor <$> Gdk.getEventButtonY eventButton
                  window <- Gtk.widgetGetToplevel drawingArea
                  -- (_, widgetX, widgetY) <- Gtk.widgetTranslateCoordinates window drawingArea eventX eventY
                  liftIO $ handleEvent $ f (fromIntegral eventX / width , fromIntegral eventY / height)
              pure False
            pure ()
        )
      |-> simpleRun (\(AspectRatio ratio) -> liftIO $ writeIORef aspectRef $ Just ratio)
    runRenderWithContext :: GIC.Context -> C.Render () -> IO ()
    runRenderWithContext ct r = GIC.withManagedPtr ct $ \p ->
      runReaderT (C.runRender r) (C.Cairo (castPtr p))
