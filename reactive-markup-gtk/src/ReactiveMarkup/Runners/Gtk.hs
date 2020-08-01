{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ReactiveMarkup.Runners.Gtk
  ( GtkElements,
    GtkRootMarkup,
    widgetRunner,
    basicGtkSetup,
    toWidget,
  )
where

import Control.Monad (when, join, replicateM_)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as R
import Data.Colour
import Data.Colour.SRGB
import Data.Functor ((<&>))
import qualified Data.GI.Base.Attributes as Gtk (AttrOpTag (..))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Input
import ReactiveMarkup.Elements.Layout
import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents
import ReactiveMarkup.Elements.Options
import Data.Word (Word32)
import qualified Data.GI.Base.Signals as GLib
import Data.Proxy

type GtkElements =
  [ Label ('[Text] |-> Font),
    List '[Orientation],
    Button ('[Text, Activate, Click] |-> Font),
    DynamicState,
    DynamicMarkup,
    FlowLayout '[Orientation],
    GridLayout,
    TextInput ('[Text, TextChange, Activate] |-> Font),
    HotKey
  ]

type Font = '[FontSize, FontWeight, FontStyle, FontColour]

windowRunner :: Runner '[Window '[Text]] IO (GtkM Gtk.Widget)
windowRunner = emptyRunner
  |-> (\(Window (Options options) children) childrenRunner handleEvent -> do
    sequence $ runMarkup optionRunner handleEvent <$> options
    runMarkup childrenRunner handleEvent children
  )
  where
    optionRunner :: Runner '[Text] IO (GtkM ())
    optionRunner = emptyRunner
      |-> (\(Text t) _ _ -> ask gtkWindow >>= \w -> Gtk.set w [#title Gtk.:= t])

-- | Basic `Runner` for GTK 3. Could be improved drastically by utilizing a similar technique to virtual DOM.
-- widgetRunner :: Runner GtkElements IO (GtkM (Ret a))
widgetRunner :: Runner GtkElements IO (GtkM Gtk.Widget)
widgetRunner =
  emptyRunner
    -- basic elements
    |-> runLabel
    |-> runList
    |-> runButton
    |-> runDynamicState
    |-> runDynamicMarkup
    -- styling elements
    -- additional layouts
    |-> runFlowLayout
    |-> runGridLayout
    -- input elements
    |-> runTextInput
    |-> runHotKey

runFontOptions :: forall result. (T.Text -> result) -> Runner Font IO result
runFontOptions f = emptyRunner |-> runFontSize |-> runFontWeight |-> runFontStyle |-> runFontColour
  where 
    runFontSize :: RunElement FontSize IO result
    runFontSize fontSize _ _ = f $ 
      let sizeText = case fontSize of
            FontPixel i -> T.pack (show i) <> "px"
            FontPercent i -> T.pack (show i) <> "%"
      in "font-size: " <> sizeText <> ";"
    runFontWeight :: RunElement FontWeight IO result
    runFontWeight (FontWeight weight) _ _ = f $ "font-weight: " <> T.pack (show weight) <> ";"
    runFontStyle :: RunElement FontStyle IO result
    runFontStyle fontStyle _ _ = f $
      let styleText = case fontStyle of
            RegularStyle -> ""
            ItalicStyle -> "font-style: italic;"
      in styleText
    runFontColour :: RunElement FontColour IO result
    runFontColour (FontColour colour) _ _ = f $ "color:" <> T.pack (sRGB24show colour) <> ";"

runLabel :: RunElement (Label ('[Text] |-> Font)) IO (GtkM Gtk.Widget)
runLabel (Label (Options options)) _ handleEvent = do
  label <- Gtk.new Gtk.Label []
  widget <- Gtk.toWidget label
  css <- fmap T.concat $ sequenceA $
            runMarkup (optionRunner label) handleEvent <$> options
  addCssToWidget widget css
  pure widget
  where
    optionRunner :: Gtk.Label -> Runner ('[Text] |-> Font) IO (GtkM T.Text)
    optionRunner label = mergeRunners (textRunner label) (runFontOptions (pure))
    textRunner :: Gtk.Label -> Runner '[Text] IO (GtkM T.Text)
    textRunner label = emptyRunner |-> (\(Text t) _ _ -> Gtk.set label [#label Gtk.:= t] *> pure "")

runList :: RunElement (List '[Orientation]) IO (GtkM Gtk.Widget)
runList (List (Options options) children) runner handleEvent = do
  let gtkOptions = runMarkup (emptyRunner |-> runOrientation) handleEvent <$> options
  boxLayout <- Gtk.new Gtk.Box ((#orientation Gtk.:= Gtk.OrientationVertical):gtkOptions)
  sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> children
  Gtk.toWidget boxLayout
  where
    runOrientation :: RunElement (Orientation) IO (Gtk.AttrOp Gtk.Box 'Gtk.AttrConstruct)
    runOrientation (orientation) runner handleEvent =
      case orientation of
        Vertical -> #orientation Gtk.:= Gtk.OrientationVertical
        Horizontal -> #orientation Gtk.:= Gtk.OrientationHorizontal

runButton :: RunElement (Button ('[Text, Activate, Click] |-> Font)) IO (GtkM Gtk.Widget)
runButton (Button (Options options)) runner handleEvent = do
  button <- Gtk.new Gtk.Button []
  css <- fmap T.concat $ sequence $ runMarkup (optionsRunner button) handleEvent <$> options
  widget <- Gtk.toWidget button
  when (css /= "") $ addCssToWidget widget css
  pure widget
  where 
    optionsRunner :: Gtk.Button -> Runner ('[Text,Activate, Click] |-> Font) IO (GtkM T.Text)
    optionsRunner gtkButton = flip mergeRunners (runFontOptions pure) $ emptyRunner
      |-> (\(Text t) _ _ -> Gtk.set gtkButton [#label Gtk.:= t] *> pure "")
      |-> (\(Activate e) _ handleEvent -> Gtk.on gtkButton #activate (handleEvent e) *> pure "")
      |-> (\(Click e) _ handleEvent -> Gtk.on gtkButton #clicked (handleEvent e) *> pure "")


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

runFlowLayout :: RunElement (FlowLayout '[Orientation]) IO (GtkM Gtk.Widget)
runFlowLayout (FlowLayout (Options options) children) runner handleEvent = do
  let gtkOptions = runMarkup (emptyRunner |-> runOrientation) handleEvent <$> options
  flowLayout <- Gtk.new Gtk.FlowBox gtkOptions
  sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add flowLayout) <$> children
  Gtk.toWidget flowLayout
  where
    runOrientation :: RunElement (Orientation) IO (Gtk.AttrOp Gtk.FlowBox 'Gtk.AttrConstruct)
    runOrientation (orientation) runner handleEvent =
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

runTextInput :: RunElement (TextInput ('[Text, TextChange, Activate] |-> Font)) IO (GtkM Gtk.Widget)
runTextInput (TextInput (Options options)) runner handleEvent = do
  gtkEntry <- Gtk.new Gtk.Entry []
  css <- fmap T.concat $ sequenceA $ runMarkup (optionsRunner gtkEntry) handleEvent <$> options
  widget <- Gtk.toWidget gtkEntry
  addCssToWidget widget css
  pure widget
  where
    optionsRunner :: Gtk.Entry -> Runner ('[Text, TextChange, Activate] |-> Font) IO (GtkM T.Text)
    optionsRunner entry = flip mergeRunners (runFontOptions pure) $ emptyRunner 
      |-> (\(Text t) _ _ -> Gtk.set entry [#text Gtk.:= t] *> pure "")
      |-> (\(TextChange f) _ handleEvent -> Gtk.on entry #changed (Gtk.entryGetText entry >>= handleEvent . f) *> pure "")
      |-> (\(Activate e) _ handleEvent -> Gtk.on entry #activate (handleEvent e) *> pure "")

runHotKey :: RunElement HotKey IO (GtkM Gtk.Widget)
runHotKey (HotKey f child) runner handleEvent = do
  widget <- runMarkup runner handleEvent child
  eventControllerKey <- ask gtkEventControllerKey
  handler <- Gtk.onEventControllerKeyKeyPressed eventControllerKey $ mapCallback handleEvent f
  Gtk.on widget #destroy $ GLib.disconnectSignalHandler eventControllerKey handler
  pure widget
  where 
    mapCallback :: (e -> IO ()) -> (Key -> [Modifier] -> Maybe e) -> Word32 -> Word32 -> [Gdk.ModifierType] -> IO Bool
    mapCallback handleEvent f gtkKeyval gtkKeycode gtkModifiers = 
      let modifiers = foldr (mapModifier) [] gtkModifiers
      in case mapKey gtkKeyval >>= \k -> f k modifiers of
        Just e -> handleEvent e *> pure True
        Nothing -> pure False
    mapModifier :: Gdk.ModifierType -> [Modifier] -> [Modifier]
    mapModifier Gdk.ModifierTypeControlMask = (ModControl :)
    mapModifier Gdk.ModifierTypeMod1Mask = (ModAlt :)
    mapModifier Gdk.ModifierTypeShiftMask = (ModShift :)
    mapModifier Gdk.ModifierTypeSuperMask = (ModSuper :)
    mapModifier _ = id
    mapKey :: Word32 -> Maybe Key
    mapKey gtkKey = case gtkKey of
      Gdk.KEY_q -> Just KeyQ
      Gdk.KEY_Q -> Just KeyQ
      Gdk.KEY_w -> Just KeyW
      Gdk.KEY_W -> Just KeyW
      Gdk.KEY_e -> Just KeyE
      Gdk.KEY_E -> Just KeyE
      Gdk.KEY_r -> Just KeyR
      Gdk.KEY_R -> Just KeyR
      Gdk.KEY_t -> Just KeyT
      Gdk.KEY_T -> Just KeyT
      Gdk.KEY_z -> Just KeyZ
      Gdk.KEY_Z -> Just KeyZ
      Gdk.KEY_u -> Just KeyU
      Gdk.KEY_Z -> Just KeyU
      Gdk.KEY_i -> Just KeyI
      Gdk.KEY_I -> Just KeyI
      Gdk.KEY_o -> Just KeyO
      Gdk.KEY_O -> Just KeyO
      Gdk.KEY_p -> Just KeyP
      Gdk.KEY_P -> Just KeyP
      Gdk.KEY_a -> Just KeyA
      Gdk.KEY_A -> Just KeyA
      Gdk.KEY_s -> Just KeyS
      Gdk.KEY_S -> Just KeyS
      Gdk.KEY_d -> Just KeyD
      Gdk.KEY_D -> Just KeyD
      Gdk.KEY_f -> Just KeyF
      Gdk.KEY_F -> Just KeyF
      Gdk.KEY_j -> Just KeyJ
      Gdk.KEY_J -> Just KeyJ
      Gdk.KEY_k -> Just KeyK
      Gdk.KEY_K -> Just KeyK
      Gdk.KEY_l -> Just KeyL
      Gdk.KEY_L -> Just KeyL
      Gdk.KEY_y -> Just KeyY
      Gdk.KEY_Y -> Just KeyY
      Gdk.KEY_x -> Just KeyX
      Gdk.KEY_X -> Just KeyX
      Gdk.KEY_c -> Just KeyC
      Gdk.KEY_C -> Just KeyC
      Gdk.KEY_v -> Just KeyV
      Gdk.KEY_V -> Just KeyV
      Gdk.KEY_b -> Just KeyB
      Gdk.KEY_B -> Just KeyB
      Gdk.KEY_n -> Just KeyN
      Gdk.KEY_N -> Just KeyN
      Gdk.KEY_m -> Just KeyM
      Gdk.KEY_M -> Just KeyM
      Gdk.KEY_ISO_Enter -> Just KeyEnter
      Gdk.KEY_space -> Just KeySpace
      _ -> Nothing


defaultGtkState :: Gtk.Window -> IO GtkState
defaultGtkState window = do
  idRef <- newIORef 0
  eventControllerKey <- Gtk.eventControllerKeyNew window
  -- styleContext <- Gtk.styleContextNew
  -- colour <- getColor styleContext
  pure $ GtkState idRef window eventControllerKey
    where 
      getColor :: Gtk.StyleContext -> IO (Colour Double)
      getColor styleContext = do
        gtkColour <- Gtk.styleContextGetColor styleContext []
        r <- Gdk.getRGBARed gtkColour
        g <- Gdk.getRGBAGreen gtkColour
        b <- Gdk.getRGBABlue gtkColour
        a <- Gdk.getRGBAAlpha gtkColour
        pure (sRGB r g b)
        
data GtkState = GtkState
  { gtkId :: IORef Int
  , gtkWindow :: Gtk.Window
  , gtkEventControllerKey :: Gtk.EventControllerKey
  }

-- | Quick setup for GTKM to display a widget generated by 'runMarkup' in combination with 'widgetRunner'.
--
-- Example: main = basicGtkSetup "MyWindowName" $ runMarkup widgetRunner (\_ -> pure ()) myMarkup
basicGtkSetup :: (SubList children GtkElements, GtkRootMarkup elems) => T.Text -> (e -> IO ()) -> Markup elems children e -> IO ()
basicGtkSetup windowTitle handleEvent markup = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window [#title Gtk.:= windowTitle]
  Gtk.on win #destroy Gtk.mainQuit
  gtkState <- defaultGtkState win
  widget <- toWidget win handleEvent markup
  Gtk.containerAdd win widget
  #showAll win
  Gtk.main

-- | Transforms the given markup into IO action resulting in a Gtk widget.
toWidget :: forall elems children e.
  (SubList children GtkElements, GtkRootMarkup elems) =>
  Gtk.Window ->
  (e -> IO ()) ->
  Markup elems children e ->
  IO Gtk.Widget
toWidget window handleEvent markup = do
  gtkState <- defaultGtkState window
  runGtkM gtkState $ 
    runGtkMarkup handleEvent markup

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

class GtkRootMarkup elems where
  runGtkMarkup :: SubList children GtkElements => (e -> IO ()) -> Markup elems children e -> GtkM Gtk.Widget

instance GtkRootMarkup' (IsWindow elems) elems => GtkRootMarkup elems where
  runGtkMarkup = runGtkMarkup' (Proxy @(IsWindow elems))

class GtkRootMarkup' (hasWindow :: Bool) elems where 
  runGtkMarkup' :: SubList children GtkElements => Proxy hasWindow -> (e -> IO ()) -> Markup elems children e -> GtkM Gtk.Widget

instance GtkRootMarkup' 'True '[Window '[Text]] where
  runGtkMarkup' _ handleEvent markup = runMarkupWithTwo windowRunner widgetRunner handleEvent markup

instance SubList elems GtkElements => GtkRootMarkup' 'False elems where
  runGtkMarkup' _ handleEvent markup = runMarkupWithTwo widgetRunner widgetRunner handleEvent markup

type family IsWindow elems :: Bool where
  IsWindow (Window _:as) = 'True
  IsWindow (a:as) = IsWindow as
  IsWindow '[] = 'False