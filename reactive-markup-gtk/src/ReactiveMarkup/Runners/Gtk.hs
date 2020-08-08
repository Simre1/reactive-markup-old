{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ReactiveMarkup.Runners.Gtk
  ( GtkElements,
    GtkRootMarkup,
    widgetRunner,
    windowRunner,
    basicGtkSetup,
    toWidget,
    GtkM,
    GtkState(..),
    withCustomRunner
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
import qualified Data.GI.Base.Overloading as GI
import qualified Data.GI.Base.Attributes as GI

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
  [ Label ('[Text] |-> BasicStyling),
    List ('[Orientation] |-> Expandable),
    Button ('[Text, Activate, Click] |-> Expandable |-> BasicStyling),
    DynamicState,
    DynamicStateIO,
    DynamicMarkup,
    HandleEvent,
    HandleEventIO,
    FlowLayout '[Orientation],
    GridLayout [HomogenousRows, HomogenousColumns],
    TextInput ('[Text, TextChange, Activate] |-> BasicStyling),
    HotKey
  ]

type Expandable = '[HorizontalExpand, VerticalExpand]

type BasicStyling = '[FontSize, FontWeight, FontStyle, FontColour, BackgroundColour]

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
    |-> runDynamicStateIO
    |-> runDynamicMarkup
    |-> runHandleEvent
    |-> runHandleEventIO
    -- additional layouts
    |-> runFlowLayout
    |-> runGridLayout
    -- input elements
    |-> runTextInput
    |-> runHotKey

runLabel :: RunElement (Label ('[Text] |-> BasicStyling)) IO (GtkM Gtk.Widget)
runLabel (Label options) _ handleEvent = do
  label <- Gtk.new Gtk.Label []
  widget <- Gtk.toWidget label
  applyOptions label options handleEvent $ runBasicStyling . runTextLabel
  pure widget

runList :: RunElement (List ('[Orientation] |-> Expandable)) IO (GtkM Gtk.Widget)
runList (List options children) runner handleEvent = do
  boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
  applyOptions boxLayout options handleEvent $ runExpandable . runOrientation
  sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> children
  Gtk.toWidget boxLayout


-- runBox :: RunElement (Box [MinWidth, MinHeight]) IO (GtkM Gtk.Widget)
-- runBox (Box (Options options) child) runner handleEvent = do
--   gtkBox <- Gtk.new Gtk.Box [#expand Gtk.:= True]
--   childWidget <- runMarkup runner handleEvent child
--   #add gtkBox childWidget
--   widget <- Gtk.toWidget gtkBox
--   addCssToWidget widget $ T.concat $ runMarkup optionsRunner handleEvent <$> options
--   pure widget
--   where 
--     optionsRunner :: Runner [MinWidth, MinHeight] IO (T.Text)
--     optionsRunner = emptyRunner
--       |-> (\(MinWidth u) _ _ -> "min-width:" <> toCssUnit u <> ";")
--       |-> (\(MinHeight u) _ _ -> "min-height:" <> toCssUnit u <> ";")
--       where 
--         toCssUnit (Pixel p) = T.pack (show p) <> "px"
--         toCssUnit (Percent p) = T.pack (show p) <> "%"

runButton :: RunElement (Button ('[Text, Activate, Click] |-> Expandable |-> BasicStyling)) IO (GtkM Gtk.Widget)
runButton (Button options) runner handleEvent = do
  button <- Gtk.new Gtk.Button []
  applyOptions button options handleEvent $ runBasicStyling . runExpandable . runClick . runActivate . runTextLabel
  widget <- Gtk.toWidget button
  pure widget

runDynamicState :: RunElement DynamicState IO (GtkM Gtk.Widget)
runDynamicState (DynamicState state mapEvent generateMarkup) childRunner handleOuterEvent = do
  (dynamicState, updateState) <- liftIO $ newDynamic state
  let handleInnerEvent innerEvent = do
        state <- current $ toBehavior dynamicState
        let (changedState, outerEvent) = mapEvent state innerEvent
        maybe (pure ()) (triggerEvent updateState) changedState
        maybe (pure ()) handleOuterEvent outerEvent
  runMarkup childRunner handleInnerEvent $ generateMarkup dynamicState

runDynamicStateIO :: RunElement DynamicStateIO IO (GtkM Gtk.Widget)
runDynamicStateIO (DynamicStateIO state mapEvent generateMarkup) childRunner handleOuterEvent = do
  (dynamicState, updateState) <- liftIO $ newDynamic state
  let handleInnerEvent innerEvent = do
        state <- current $ toBehavior dynamicState
        (changedState, outerEvent) <- liftIO $ mapEvent state innerEvent
        maybe (pure ()) (triggerEvent updateState) changedState
        maybe (pure ()) handleOuterEvent outerEvent
  runMarkup childRunner handleInnerEvent $ generateMarkup dynamicState

runDynamicMarkup :: RunElement DynamicMarkup IO (GtkM Gtk.Widget)
runDynamicMarkup (DynamicMarkup dynamicState generateMarkup) childRunner handleEvent = do
  boxLayout <- Gtk.new Gtk.Box [#expand Gtk.:= True, #orientation Gtk.:= Gtk.OrientationVertical]
  state <- liftIO $ current $ toBehavior dynamicState
  cleanUpRef <- liftIO $ newIORef (pure ())
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
      reactimate (toEvent dynamicState) $ simpleEventHandler handler
  generateWidget state >>= setWidget
  widget <- Gtk.toWidget boxLayout
  Gtk.on widget #destroy (liftES unregisterWidgetUpdate)
  pure widget

runHandleEvent :: RunElement HandleEvent IO (GtkM Gtk.Widget)
runHandleEvent (HandleEvent innerHandle markup) runner handleEvent = do
  runMarkup runner (maybe (pure ()) handleEvent . innerHandle) markup

runHandleEventIO :: RunElement HandleEventIO IO (GtkM Gtk.Widget)
runHandleEventIO (HandleEventIO innerHandle markup) runner handleEvent = do
  runMarkup runner (\x -> innerHandle x >>= maybe (pure ()) handleEvent) markup

runFlowLayout :: RunElement (FlowLayout '[Orientation]) IO (GtkM Gtk.Widget)
runFlowLayout (FlowLayout options children) runner handleEvent = do
  flowLayout <- Gtk.new Gtk.FlowBox []
  applyOptions flowLayout options handleEvent $ runOrientation
  sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add flowLayout) <$> children
  Gtk.toWidget flowLayout

runGridLayout :: RunElement (GridLayout [HomogenousRows, HomogenousColumns]) IO (GtkM Gtk.Widget)
runGridLayout (GridLayout options children) runner handleEvent = do
  gridLayout <- Gtk.new Gtk.Grid []
  applyOptions gridLayout options handleEvent $ runHomogenousColumns . runHomogenousRows
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

runTextInput :: RunElement (TextInput ('[Text, TextChange, Activate] |-> BasicStyling)) IO (GtkM Gtk.Widget)
runTextInput (TextInput options) runner handleEvent = do
  gtkEntry <- Gtk.new Gtk.Entry []
  applyOptions gtkEntry options handleEvent $ runBasicStyling . runActivate . runTextChange . runTextText
  Gtk.toWidget gtkEntry

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

type GtkOption o = GtkM (T.Text, [Gtk.AttrOp o Gtk.AttrSet], o -> GtkM ())

type AllowedOp o s r = GI.AttrSetC (GI.ResolveAttribute s o) o s r

type AllowedSignal o s info = (GLib.SignalInfo info, Gtk.GObject o, info ~ GI.ResolveSignal s o, GLib.HaskellCallbackType info ~ IO ())

applyOptions :: Gtk.IsWidget o => o -> Options options e -> (e -> IO ()) -> (Runner '[] IO (GtkOption o) -> Runner options IO (GtkOption o)) -> GtkM ()
applyOptions widget (Options options) handleEvent makeRunner = do
  gtkOptions <- sequenceA $ runMarkupExact (makeRunner emptyRunner) handleEvent <$> options
  let css = T.concat $ (\(a,_,_) -> a) <$> gtkOptions
      attributes = concat $ (\(_,a,_) -> a) <$> gtkOptions
      action = sequenceA $ (\(_,_,a) -> a widget) <$> gtkOptions
  Gtk.set widget attributes
  addCssToWidget widget css
  action
  pure ()


runBasicStyling :: Runner elems IO (GtkOption o) -> Runner (elems |-> '[FontSize] |-> '[FontWeight] |-> '[FontStyle] |-> '[FontColour] |-> '[BackgroundColour]) IO (GtkOption o)
runBasicStyling = runBackgroundColour . runFontColour . runFontStyle . runFontWeight . runFontSize

runTextLabel :: AllowedOp o "label" T.Text => Runner elems IO (GtkOption o) -> Runner (elems |-> '[Text]) IO (GtkOption o)
runTextLabel runner = runner |-> (\(Text t) _ _ -> pure ("", [#label Gtk.:= t], const (pure ())))

runTextText :: AllowedOp o "text" T.Text => Runner elems IO (GtkOption o) -> Runner (elems |-> '[Text]) IO (GtkOption o)
runTextText runner = runner |-> (\(Text t) _ _ -> pure ("", [#text Gtk.:= t], const (pure ())))

runOrientation :: AllowedOp o "orientation" Gtk.Orientation => Runner elems IO (GtkOption o) -> Runner (elems |-> '[Orientation]) IO (GtkOption o)
runOrientation runner = runner |-> (\orientation _ _ -> case orientation of
  Horizontal -> pure ("", [#orientation Gtk.:= Gtk.OrientationHorizontal], const (pure ()))
  Vertical -> pure ("", [#orientation Gtk.:= Gtk.OrientationVertical], const (pure ()))
  )

runFontSize :: Runner elems IO (GtkOption o) -> Runner (elems |-> '[FontSize]) IO (GtkOption o)
runFontSize runner = runner |-> \(FontSize unit) _ _ ->
  let sizeText = case unit of
        Pixel i -> T.pack (show i) <> "px"
        Percent i -> T.pack (show i) <> "%"
  in pure ("font-size: " <> sizeText <> ";", [], const (pure ()))

runFontWeight :: Runner elems IO (GtkOption o) -> Runner (elems |-> '[FontWeight]) IO (GtkOption o)
runFontWeight runner = runner |-> \(FontWeight weight) _ _ -> pure ("font-weight: " <> T.pack (show weight) <> ";", [], const (pure ()))

runFontStyle :: Runner elems IO (GtkOption o) -> Runner (elems |-> '[FontStyle]) IO (GtkOption o)
runFontStyle runner = runner |-> \fontStyle _ _ ->
  let styleText = case fontStyle of
        RegularStyle -> ""
        ItalicStyle -> "font-style: italic;"
  in pure (styleText, [], const (pure ()))

runFontColour :: Runner elems IO (GtkOption o) -> Runner (elems |-> '[FontColour]) IO (GtkOption o)
runFontColour runner = runner |-> \(FontColour colour) _ _ -> pure ("color:" <> T.pack (sRGB24show colour) <> ";", [], const (pure ()))

runBackgroundColour :: Runner elems IO (GtkOption o) -> Runner (elems |-> '[BackgroundColour]) IO (GtkOption o)
runBackgroundColour runner = runner |-> \(BackgroundColour colour) _ _ -> pure ("background-color:" <> T.pack (sRGB24show colour) <> ";", [], const (pure ()))

runHorizontalExpand :: AllowedOp o "hexpand" Bool => Runner elems IO (GtkOption o) -> Runner (elems |-> '[HorizontalExpand]) IO (GtkOption o)
runHorizontalExpand runner = runner |-> (\(HorizontalExpand b) _ _ -> pure ("", [#hexpand Gtk.:= b], const (pure ())))

runVerticalExpand :: AllowedOp o "vexpand" Bool => Runner elems IO (GtkOption o) -> Runner (elems |-> '[VerticalExpand]) IO (GtkOption o)
runVerticalExpand runner = runner |-> (\(VerticalExpand b) _ _ -> pure ("", [#vexpand Gtk.:= b], const (pure ())))

runExpandable :: (AllowedOp o "vexpand" Bool, AllowedOp o "hexpand" Bool) => Runner elems IO (GtkOption o) -> Runner (elems |-> '[HorizontalExpand] |-> '[VerticalExpand]) IO (GtkOption o)
runExpandable = runVerticalExpand . runHorizontalExpand

runActivate :: AllowedSignal o "activate" info => Runner elems IO (GtkOption o) -> Runner (elems |-> '[Activate]) IO (GtkOption o)
runActivate runner = runner |-> (\(Activate e) _ handleEvent -> pure ("", [], \o -> Gtk.on o #activate (handleEvent e) *> pure ()))

runClick :: AllowedSignal o "clicked" info => Runner elems IO (GtkOption o) -> Runner (elems |-> '[Click]) IO (GtkOption o)
runClick runner = runner |-> (\(Click e) _ handleEvent -> pure ("", [], \o -> Gtk.on o #clicked (handleEvent e) *> pure ()))

runHomogenousRows :: AllowedOp o "rowHomogeneous" Bool => Runner elems IO (GtkOption o) -> Runner (elems |-> '[HomogenousRows]) IO (GtkOption o)
runHomogenousRows runner = runner |-> (\HomogenousRows _ _ -> pure ("", [#rowHomogeneous Gtk.:= True], const (pure ())))

runHomogenousColumns :: AllowedOp o "columnHomogeneous" Bool => Runner elems IO (GtkOption o) -> Runner (elems |-> '[HomogenousColumns]) IO (GtkOption o)
runHomogenousColumns runner = runner |-> (\HomogenousColumns _ _ -> pure ("", [#columnHomogeneous Gtk.:= True], const (pure ())))

runTextChange :: (Gtk.IsEntry o, AllowedSignal o "changed" info) => Runner elems IO (GtkOption o) -> Runner (elems |-> '[TextChange]) IO (GtkOption o)
runTextChange runner = runner |-> (\(TextChange f) _ handleEvent -> pure ("", [], \o -> Gtk.on o #changed (Gtk.entryGetText o >>= handleEvent . f) *> pure ()))

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
basicGtkSetup :: (SubList children GtkElements, GtkRootMarkup elems) => (e -> IO ()) -> Markup elems children e -> IO ()
basicGtkSetup handleEvent markup = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window [#title Gtk.:= "My App"]
  Gtk.on win #destroy Gtk.mainQuit
  gtkState <- defaultGtkState win
  widget <- toWidget win handleEvent markup
  Gtk.containerAdd win widget
  #showAll win
  Gtk.main

withCustomRunner :: (Markup elems children e -> GtkM Gtk.Widget) -> Markup elems children e -> IO ()
withCustomRunner f markup = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window []
  Gtk.on win #destroy Gtk.mainQuit
  gtkState <- defaultGtkState win
  widget <- runGtkM gtkState (f markup)
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

addCssToWidget :: Gtk.IsWidget o => o -> T.Text -> GtkM ()
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