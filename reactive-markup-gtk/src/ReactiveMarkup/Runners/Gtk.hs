{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ReactiveMarkup.Runners.Gtk 
  ( GtkElements
  , gtkRunner
  , basicGtkSetup
  , toWidget
  )
  where

import Control.Monad (join, replicateM_)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as R
import Data.Colour.SRGB (sRGB24show)
import Data.Colour.Names (black)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified GI.Gtk as Gtk
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Layout
import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents

type GtkElements = [Label, List, Button, DynamicState, DynamicMarkup, Set FontSize, Set FontWeight, Set FontStyle, Set Orientation, Set FontColour, FlowLayout, GridLayout]

-- | Basic `Runner` for GTK 3. Could be improved drastically by utilizing a similar technique to virtual DOM.
gtkRunner :: Runner GtkElements IO (GtkM Gtk.Widget)
gtkRunner =
  emptyRunner
    -- basic elements
    |-> runLabel
    |-> runList
    |-> runButton
    |-> runDynamicState
    |-> runDynamicMarkup
    -- styling elements
    |-> runFontSize
    |-> runFontWeight
    |-> runFontStyle
    |-> runOrientation
    |-> runFontColour
    -- additional layouts
    |-> runFlowLayout
    |-> runGridLayout

runLabel :: RunElement Label IO (GtkM Gtk.Widget)
runLabel (Label text) _ _ = do
  label <- Gtk.new Gtk.Label [#label Gtk.:= text]
  widget <- Gtk.toWidget label
  fontCss >>= addCssToWidget widget
  pure widget

runList :: RunElement List IO (GtkM Gtk.Widget)
runList (List markups) runner handleEvent = do
  boxLayout <- Gtk.new Gtk.Box []
  orientation <- ask gtkOrientation
  sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> markups
  Gtk.set boxLayout $ case orientation of
    Horizontal -> [#orientation Gtk.:= Gtk.OrientationHorizontal]
    Vertical -> [#orientation Gtk.:= Gtk.OrientationVertical]
  Gtk.toWidget boxLayout

runButton :: RunElement Button IO (GtkM Gtk.Widget)
runButton (Button text) runner handleEvent = do
  button <- Gtk.new Gtk.Button [#label Gtk.:= text]
  Gtk.on button #clicked (handleEvent Click)
  widget <- Gtk.toWidget button
  fontCss >>= addCssToWidget widget
  pure widget

runDynamicState :: RunElement DynamicState IO (GtkM Gtk.Widget)
runDynamicState (DynamicState state mapEvent generateMarkup) childRunner handleOuterEvent = do
  (dynamicState, updateState) <- liftIO $ newDynamic state
  let handleInnerEvent innerEvent = do
        state <- current $ toBehavior dynamicState
        let (changedState, outerEvent) = mapEvent state innerEvent
        maybe mempty (triggerEvent updateState) changedState
        maybe mempty handleOuterEvent outerEvent
  runMarkup childRunner handleInnerEvent $ generateMarkup dynamicState

runDynamicMarkup :: RunElement DynamicMarkup IO (GtkM Gtk.Widget)
runDynamicMarkup (DynamicMarkup dynamicState generateMarkup) childRunner handleEvent = do
  boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
  state <- liftIO $ current $ toBehavior dynamicState
  cleanUpRef <- liftIO $ newIORef mempty
  let setWidget widget = liftIO $ do
        cleanUp <- join $ readIORef cleanUpRef
        writeIORef cleanUpRef (#remove boxLayout widget)
        #add boxLayout widget
        #showAll widget
      generateWidget state = do
        runMarkup childRunner handleEvent $ generateMarkup state
  handler <- withinIO $ \newState -> generateWidget newState >>= setWidget
  unregisterWidgetUpdate <-
    liftIO $
      reactimate (toEvent dynamicState) $
        simpleEventHandler $ handler
  generateWidget state >>= setWidget
  widget <- Gtk.toWidget boxLayout
  Gtk.on widget #destroy (liftES unregisterWidgetUpdate)
  pure widget

runFontSize :: RunElement (Set FontSize) IO (GtkM Gtk.Widget)
runFontSize (Set (FontSize changeSize) markup) runner handleEvent =
  local (\s -> s {gtkFontSize = changeSize (gtkFontSize s)}) $
    runMarkup runner handleEvent markup

runFontWeight :: RunElement (Set FontWeight) IO (GtkM Gtk.Widget)
runFontWeight (Set weight markup) runner handleEvent =
  local (\s -> s {gtkFontWeight = weight}) $
    runMarkup runner handleEvent markup

runFontStyle :: RunElement (Set FontStyle) IO (GtkM Gtk.Widget)
runFontStyle (Set style markup) runner handleEvent =
  local (\s -> s {gtkFontStyle = style}) $
    runMarkup runner handleEvent markup

runOrientation :: RunElement (Set Orientation) IO (GtkM Gtk.Widget)
runOrientation (Set orientation markup) runner handleEvent =
  local (\s -> s {gtkOrientation = orientation}) $
    runMarkup runner handleEvent markup

runFontColour :: RunElement (Set FontColour) IO (GtkM Gtk.Widget)
runFontColour (Set colour markup) runner handleEvent =
  local (\s -> s {gtkFontColour = colour}) $
    runMarkup runner handleEvent markup

runFlowLayout :: RunElement FlowLayout IO (GtkM Gtk.Widget)
runFlowLayout (FlowLayout children) runner handleEvent = do
  flowLayout <- Gtk.new Gtk.FlowBox []
  orientation <- ask gtkOrientation
  sequenceA $ (\child -> runMarkup runner handleEvent child >>= #add flowLayout) <$> children
  Gtk.set flowLayout $ case orientation of
    Horizontal -> [#orientation Gtk.:= Gtk.OrientationHorizontal]
    Vertical -> [#orientation Gtk.:= Gtk.OrientationVertical]
  Gtk.toWidget flowLayout

runGridLayout :: RunElement GridLayout IO (GtkM Gtk.Widget)
runGridLayout (GridLayout gridOptions children) runner handleEvent = do
  gridLayout <- Gtk.new Gtk.Grid []
  replicateM_ (gridRows gridOptions) $ Gtk.gridInsertRow gridLayout 0
  replicateM_ (gridColumns gridOptions) $ Gtk.gridInsertColumn gridLayout 0
  sequence $ runMarkupWithTwoExact (emptyRunner |-> gridChildRunner gridLayout) runner handleEvent <$> children
  Gtk.toWidget gridLayout
  where
    gridChildRunner :: Gtk.Grid -> RunElement GridChild IO (GtkM Gtk.Widget)
    gridChildRunner gridLayout (GridChild childOptions markup) runner handleEvent = do
        child <- runMarkup runner handleEvent markup
        Gtk.gridAttach gridLayout child
            (fromIntegral $ gridChildColumn childOptions)
            (fromIntegral $ gridChildRow childOptions)
            (fromIntegral $ gridChildWidth childOptions)
            (fromIntegral $ gridChildHeight childOptions)
        pure child

fontCss :: GtkM T.Text
fontCss = foldl (<>) "" <$> sequenceA [ask (fontColour . gtkFontColour), ask (fontSize . gtkFontSize), ask (fontStyle . gtkFontStyle), ask (fontWeight . gtkFontWeight)]
  where
    fontColour (FontColour colour) = "color:" <> T.pack (sRGB24show colour) <> ";"
    fontSize size = "font-size:" <> T.pack (show size) <> "px;"
    fontStyle style =
      let styleText = case style of
            RegularStyle -> "normal"
            ItalicStyle -> "italic"
       in "font-style:" <> styleText <> ";"
    fontWeight (FontWeight weight) = "font-weight:" <> T.pack (show weight) <> ";"

defaultGtkState :: IO GtkState
defaultGtkState = newIORef 0 <&> \ref -> GtkState Vertical (FontColour black) (FontWeight 400) RegularStyle 15 ref

data GtkState = GtkState
  { gtkOrientation :: Set Orientation,
    gtkFontColour :: Set FontColour,
    gtkFontWeight :: Set FontWeight,
    gtkFontStyle :: Set FontStyle,
    gtkFontSize :: Int,
    gtkId :: IORef Int
  }

-- | Quick setup for GTKM to display a widget generated by 'runMarkup' in combination with 'gtkRunner'.
--
-- Example: main = basicGtkSetup "MyWindowName" $ runMarkup gtkRunner (\_ -> pure ()) myMarkup
basicGtkSetup :: (SubList (Merge elems children) GtkElements) => T.Text -> (e -> IO ()) -> Markup elems children e -> IO ()
basicGtkSetup windowTitle handleEvent markup = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window [#title Gtk.:= windowTitle]
  Gtk.on win #destroy Gtk.mainQuit
  widget <- toWidget handleEvent markup
  Gtk.containerAdd win widget
  #showAll win
  Gtk.main

-- | Transforms the given markup into IO action resulting in a Gtk widget.
toWidget ::
  (SubList (Merge elems children) GtkElements) =>
  (e -> IO ()) ->
  Markup elems children e ->
  IO Gtk.Widget
toWidget handleEvent markup = do
  gtkState <- defaultGtkState
  runGtkM gtkState $ runMarkup gtkRunner handleEvent markup

newtype GtkM a = GtkM (R.ReaderT GtkState IO a) deriving (Functor, Applicative, Monad, MonadIO)

ask :: (GtkState -> a) -> GtkM a
ask f = GtkM $ f <$> R.ask

local :: (GtkState -> GtkState) -> GtkM a -> GtkM a
local f (GtkM s) = GtkM $ R.local f s

nextId :: GtkM Int
nextId = do
  ref <- ask gtkId
  i <- liftIO $ atomicModifyIORef ref (\i -> (succ i, i))
  pure i

withinIO :: (s -> GtkM a) -> GtkM (s -> IO a)
withinIO f = do
  state <- ask id
  pure $ \s -> do
    a <- runGtkM state (f s)
    pure a

runGtkM :: GtkState -> GtkM a -> IO a
runGtkM state (GtkM s) = R.runReaderT s state

addCssToWidget :: Gtk.Widget -> T.Text -> GtkM ()
addCssToWidget widget css = do
  i <- nextId
  let widgetName = "e" <> T.pack (show i)
  cssProvider <- Gtk.cssProviderNew
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.widgetSetName widget widgetName
  Gtk.cssProviderLoadFromData cssProvider $ T.encodeUtf8 ("#" <> widgetName <> "{" <> css <> "}")
  Gtk.styleContextAddProvider styleContext cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
