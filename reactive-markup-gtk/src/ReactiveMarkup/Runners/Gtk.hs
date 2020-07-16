{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReactiveMarkup.Runners.Gtk where

import qualified Control.Monad.Trans.Reader as R
import Data.Colour.SRGB (Colour, RGB (RGB), sRGB24show, toSRGB)
import qualified Data.GI.Base.GValue as GI
import Data.IORef
import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word16)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as P
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Colour (black)

-- | Basic `Runner` for GTK 3. It is definitely not optimal, but is sufficient as demontration.
gtkRunner :: Runner [Label, List, Button, DynamicState, DynamicMarkup, Set FontSize, Set FontWeight, Set FontStyle, Set Orientation, Set FontColour] IO (Gtk Gtk.Widget)
gtkRunner =
  emptyRunner
    |-> ( \(Label text) _ _ -> liftIO $ do
            label <- Gtk.new Gtk.Label [#label Gtk.:= text]
            Gtk.toWidget label
        )
    |-> ( \(List markups) runner handleEvent -> do
            boxLayout <- Gtk.new Gtk.Box []
            orientation <- get gtkOrientation
            sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> markups
            Gtk.set boxLayout $ case orientation of
              Horizontal -> [#orientation Gtk.:= Gtk.OrientationHorizontal]
              Vertical -> [#orientation Gtk.:= Gtk.OrientationVertical]
            Gtk.toWidget boxLayout
        )
    |-> ( \(Button text) runner handleEvent -> do
            button <- Gtk.new Gtk.Button [#label Gtk.:= text]
            Gtk.on button #clicked (handleEvent Click)
            Gtk.toWidget button
        )
    |-> ( \(DynamicState state mapEvent generateMarkup) childRunner handleOuterEvent -> do
            (dynamicState, updateState) <- liftIO $ newDynamic state
            let handleInnerEvent innerEvent = do
                  state <- current $ toBehavior dynamicState
                  let (changedState, outerEvent) = mapEvent state innerEvent
                  maybe mempty (triggerEvent updateState) changedState
                  maybe mempty handleOuterEvent outerEvent
            runMarkup childRunner handleInnerEvent $ generateMarkup dynamicState
        )
    |-> ( \(DynamicMarkup dynamicState generateMarkup) childRunner handleEvent -> do
            boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
            state <- liftIO $ current $ toBehavior dynamicState
            cleanUpRef <- liftIO $ newIORef mempty
            let setWidget widget = liftIO $ do
                  cleanUp <- join $ readIORef cleanUpRef
                  writeIORef cleanUpRef (#remove boxLayout widget)
                  #add boxLayout widget
                  #showAll widget
                generateWidget state =
                  runMarkup childRunner handleEvent $ generateMarkup state

            handler <- withinIO $ \newState -> generateWidget newState >>= setWidget
            unregisterWidgetUpdate <- liftIO $ reactimate (toEvent dynamicState) $
              simpleEventHandler $ handler
            generateWidget state >>= setWidget
            widget <- Gtk.toWidget boxLayout
            Gtk.on widget #destroy (liftES unregisterWidgetUpdate)
            pure widget
        )
    |-> ( \(Set (FontSize f) markup) runner handleEvent -> do
            widget <- runMarkup runner handleEvent markup
            fontSize <- f <$> get gtkFontSize
            i <- nextId
            let css =  "{font-size:" <> T.pack (show fontSize) <> "px;}"
            addCssToWidget widget i css
            pure widget
        )
    |-> (\(Set weight markup) runner handleEvent -> do
        widget <- runMarkup runner handleEvent markup
        i <- nextId
        let toWeight BoldWeight = "bold"
            toWeight RegularWeight = "bormal"
            css = "{font-weight:" <> toWeight weight <> ";}"
        addCssToWidget widget i css
        pure widget
        )
    |-> (\(Set (style) markup) runner handleEvent -> do
        widget <- runMarkup runner handleEvent markup
        i <- nextId
        let toStyle ItalicStyle = "italic"
            toStyle RegularStyle = "normal"
            css = "{font-style:" <> toStyle style <> ";}"
        addCssToWidget widget i css
        pure widget
        )
    |-> (\(Set orientation markup) runner handleEvent -> do
      widget <- runMarkup runner handleEvent markup
      modify (\s -> s{gtkOrientation=orientation})
      pure widget
      )
    |-> runnerFontColour

runnerFontColour :: AddToRunner (Set FontColour) IO (Gtk Gtk.Widget)
runnerFontColour = (\(Set (FontColour colour) markup) runner handleEvent -> do
      widget <- runMarkup runner handleEvent markup
      i <- nextId
      let css = "{color:" <> T.pack (sRGB24show colour) <> ";}"
      addCssToWidget widget i css
      pure widget
      )

newtype Gtk a = Gtk (R.ReaderT (IORef GtkState) IO a) deriving (Functor, Applicative, Monad, MonadIO)

get :: (GtkState -> a) -> Gtk a
get f = Gtk $ do
  ref <- R.ask
  liftIO $ f <$> readIORef ref

modify :: (GtkState -> GtkState) -> Gtk ()
modify f = Gtk $ do
  ref <- R.ask
  liftIO $ modifyIORef ref f

nextId :: Gtk Int
nextId = do
  i <- get gtkId
  modify (\g -> g{gtkId=succ i})
  pure i 

runGtk' :: IORef GtkState -> Gtk a -> IO a
runGtk' ref (Gtk s) = R.runReaderT s ref

runGtk :: GtkState -> Gtk Gtk.Widget -> IO Gtk.Widget
runGtk gtkState gtk = do
  ref <- newIORef gtkState
  (widget) <- runGtk' ref gtk
  pure widget

withinIO :: (s -> Gtk a) -> Gtk (s -> IO a)
withinIO gtk = do
  ref <- Gtk $ R.ask
  pure $ \s -> runGtk' ref (gtk s)

defaultGtkState :: GtkState
defaultGtkState = GtkState Vertical 15 0

data GtkState = GtkState
  { gtkOrientation :: Set Orientation,
    gtkFontSize :: Int,
    gtkId :: Int
  }

-- | Quick setup for GTK to display a widget generated by 'runMarkup' in combination with 'gtkRunner'.
--
-- Example: main = basicGtkSetup "MyWindowName" $ runMarkup gtkRunner (\_ -> pure ()) myMarkup
basicGtkSetup :: T.Text -> IO Gtk.Widget -> IO ()
basicGtkSetup windowTitle widget = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window [#title Gtk.:= windowTitle]
  Gtk.on win #destroy Gtk.mainQuit
  widget >>= #add win
  #showAll win
  Gtk.main

addCssToWidget :: MonadIO m => Gtk.Widget -> Int -> T.Text -> m ()
addCssToWidget widget i css = do
  let className = "e" <> T.pack (show i)
  cssProvider <- Gtk.cssProviderNew
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.cssProviderLoadFromData cssProvider $ T.encodeUtf8 ("." <> className <> css)
  Gtk.styleContextAddClass styleContext className
  Gtk.styleContextAddProvider styleContext cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
