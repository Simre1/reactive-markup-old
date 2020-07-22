{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ReactiveMarkup.Runners.Gtk
  ( GtkElements,
    gtkRunner,
    basicGtkSetup,
    toWidget,
  )
where

import Control.Monad (join, replicateM_)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as R
import Data.Colour.Names (black)
import Data.Colour.SRGB (sRGB24show)
import Data.Functor ((<&>))
import qualified Data.GI.Base.Attributes as Gtk (AttrOpTag (..))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified GI.Gtk as Gtk
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Input
import ReactiveMarkup.Elements.Layout
import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents

type GtkElements =
  [ Label,
    List,
    SpecificOptions List '[Orientation],
    Button,
    DynamicState,
    DynamicMarkup,
    GeneralOptions [FontSize, FontWeight, FontStyle, FontColour],
    FlowLayout,
    GridLayout,
    TextInput,
    SpecificOptions TextInput '[TextChange, Activate]
  ]

-- | Basic `Runner` for GTK 3. Could be improved drastically by utilizing a similar technique to virtual DOM.
-- gtkRunner :: Runner GtkElements IO (GtkM (Ret a))
gtkRunner :: Runner GtkElements IO (GtkM Gtk.Widget)
gtkRunner =
  emptyRunner
    -- basic elements
    |-> runLabel
    |-> runList
    |-> runSpecificListOptions
    |-> runButton
    |-> runDynamicState
    |-> runDynamicMarkup
    -- styling elements
    |-> runGeneralOptions
    -- additional layouts
    |-> runFlowLayout
    |-> runGridLayout
    -- input elements
    |-> runTextInput
    |-> runSpecificTextInputOptions

runLabel :: RunElement Label IO (GtkM Gtk.Widget)
runLabel (Label text) _ _ = do
  label <- Gtk.new Gtk.Label [#label Gtk.:= text]
  widget <- Gtk.toWidget label
  fontCss >>= addCssToWidget widget
  pure widget

runList :: RunElement List IO (GtkM Gtk.Widget)
runList (List markups) runner handleEvent = do
  boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
  sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> markups
  Gtk.toWidget boxLayout

runSpecificListOptions :: RunElement (SpecificOptions List '[Orientation]) IO (GtkM Gtk.Widget)
runSpecificListOptions (SpecificOptions options listMarkup) runner handleEvent = do
  let gtkOptions = runMarkup (emptyRunner |-> runOrientation) handleEvent <$> options
  runMarkupWithTwo
    ( emptyRunner |-> \(List markups) runnerInner handleEvent -> do
        boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
        Gtk.set boxLayout gtkOptions
        sequenceA $ (\markup -> runMarkup runnerInner handleEvent markup >>= #add boxLayout) <$> markups
        Gtk.toWidget boxLayout
    )
    runner
    handleEvent
    listMarkup
  where
    runOrientation :: RunElement (Orientation) IO (Gtk.AttrOp Gtk.Box 'Gtk.AttrSet)
    runOrientation (Orientation orientation) runner handleEvent =
      case orientation of
        Vertical -> #orientation Gtk.:= Gtk.OrientationVertical
        Horizontal -> #orientation Gtk.:= Gtk.OrientationHorizontal

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

runGeneralOptions :: RunElement (GeneralOptions '[FontSize, FontWeight, FontStyle, FontColour]) IO (GtkM Gtk.Widget)
runGeneralOptions (GeneralOptions options markup) runner handleEvent = do
  let optionsRunner = emptyRunner |-> runFontSize |-> runFontWeight |-> runFontStyle |-> runFontColour
      stateChanges = runMarkup optionsRunner handleEvent <$> options
  local (foldl (.) id stateChanges) $ runMarkup runner handleEvent markup
  where
    runFontSize :: RunElement (FontSize) IO (GtkState -> GtkState)
    runFontSize (FontSize (FontSize' changeSize)) runner handleEvent state =
      state {gtkFontSize = changeSize (gtkFontSize state)}
    runFontWeight :: RunElement (FontWeight) IO (GtkState -> GtkState)
    runFontWeight (FontWeight weight) runner handleEvent state =
      state {gtkFontWeight = weight}
    runFontStyle :: RunElement (FontStyle) IO (GtkState -> GtkState)
    runFontStyle (FontStyle style) runner handleEvent state =
      state {gtkFontStyle = style}
    runFontColour :: RunElement (FontColour) IO (GtkState -> GtkState)
    runFontColour (FontColour colour) runner handleEvent state =
      state {gtkFontColour = colour}

runFlowLayout :: RunElement FlowLayout IO (GtkM Gtk.Widget)
runFlowLayout (FlowLayout children) runner handleEvent = do
  flowLayout <- Gtk.new Gtk.FlowBox []
  sequenceA $ (\child -> runMarkup runner handleEvent child >>= #add flowLayout) <$> children
  Gtk.toWidget flowLayout

runSpecificFlowLayoutOptions :: RunElement (SpecificOptions FlowLayout '[Orientation]) IO (GtkM Gtk.Widget)
runSpecificFlowLayoutOptions (SpecificOptions options flowMarkup) runner handleEvent = do
  let gtkOptions = runMarkup (emptyRunner |-> runOrientation) handleEvent <$> options
  runMarkupWithTwo
    ( emptyRunner |-> \(FlowLayout markups) runnerInner handleEvent -> do
        flowLayout <- Gtk.new Gtk.FlowBox gtkOptions
        sequenceA $ (\markup -> runMarkup runnerInner handleEvent markup >>= #add flowLayout) <$> markups
        Gtk.toWidget flowLayout
    )
    runner
    handleEvent
    flowMarkup
  where
    runOrientation :: RunElement (Orientation) IO (Gtk.AttrOp Gtk.FlowBox 'Gtk.AttrConstruct)
    runOrientation (Orientation orientation) runner handleEvent =
      case orientation of
        Vertical -> #orientation Gtk.:= Gtk.OrientationVertical
        Horizontal -> #orientation Gtk.:= Gtk.OrientationHorizontal

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
      Gtk.gridAttach
        gridLayout
        child
        (fromIntegral $ gridChildColumn childOptions)
        (fromIntegral $ gridChildRow childOptions)
        (fromIntegral $ gridChildWidth childOptions)
        (fromIntegral $ gridChildHeight childOptions)
      pure child

runTextInput :: RunElement TextInput IO (GtkM Gtk.Widget)
runTextInput _ _ handleEvent = do
  entry <- Gtk.new Gtk.Entry []
  widget <- Gtk.toWidget entry
  fontCss >>= addCssToWidget widget
  pure widget

runSpecificTextInputOptions :: RunElement (SpecificOptions TextInput '[TextChange, Activate]) IO (GtkM Gtk.Widget)
runSpecificTextInputOptions (SpecificOptions options textInput) runner handleEvent = do
  let eventRunner = emptyRunner |-> runTextChange |-> runTextEnter
  let eventHooks entry = sequence $ ($entry) <$> (runMarkup eventRunner handleEvent <$> options)
  runMarkup (runner |-> runTextInput eventHooks) handleEvent textInput
  where
    runTextInput f (TextInput) _ _ = do
      entry <- Gtk.new Gtk.Entry []
      f entry
      widget <- Gtk.toWidget entry
      fontCss >>= addCssToWidget widget
      pure widget
    runTextChange (TextChange f) _ handleEvent entry = Gtk.on entry #changed (Gtk.entryGetText entry >>= handleEvent . f) *> pure ()
    runTextEnter (Activate e) _ handleEvent entry = Gtk.on entry #activate (handleEvent e) *> pure ()

fontCss :: GtkM T.Text
fontCss = foldl (<>) "" <$> sequenceA [ask (fontColour . gtkFontColour), ask (fontSize . gtkFontSize), ask (fontStyle . gtkFontStyle), ask (fontWeight . gtkFontWeight)]
  where
    fontColour (FontColour' colour) = "color:" <> T.pack (sRGB24show colour) <> ";"
    fontSize size = "font-size:" <> T.pack (show size) <> "px;"
    fontStyle style =
      let styleText = case style of
            RegularStyle -> "normal"
            ItalicStyle -> "italic"
       in "font-style:" <> styleText <> ";"
    fontWeight (FontWeight' weight) = "font-weight:" <> T.pack (show weight) <> ";"

defaultGtkState :: IO (GtkState)
defaultGtkState = newIORef 0 <&> \ref -> GtkState (FontColour' black) (FontWeight' 400) RegularStyle 15 ref

data GtkState = GtkState
  { gtkFontColour :: FontColour,
    gtkFontWeight :: FontWeight,
    gtkFontStyle :: FontStyle,
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

newtype GtkM a = GtkM (R.ReaderT (GtkState) IO a) deriving (Functor, Applicative, Monad, MonadIO)

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
