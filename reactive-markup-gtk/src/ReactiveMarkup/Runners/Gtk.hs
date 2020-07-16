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
import Data.Functor ((<&>))

-- | Basic `Runner` for GTK 3. It is definitely not optimal, but is sufficient as demontration.
gtkRunner :: Runner [Label, List, Button, DynamicState, DynamicMarkup, Set FontSize, Set FontWeight, Set FontStyle, Set Orientation, Set FontColour] IO (Gtk Gtk.Widget)
gtkRunner =
  emptyRunner
    |-> ( \(Label text) _ _ -> do
            label <- Gtk.new Gtk.Label [#label Gtk.:= text]
            i <- nextId
            widget <- Gtk.toWidget label
            fontCss >>= addCssToWidget widget i
            pure widget 
        )
    |-> ( \(List markups) runner handleEvent -> do
            boxLayout <- Gtk.new Gtk.Box []
            orientation <- ask gtkOrientation
            sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> markups
            Gtk.set boxLayout $ case orientation of
              Horizontal -> [#orientation Gtk.:= Gtk.OrientationHorizontal]
              Vertical -> [#orientation Gtk.:= Gtk.OrientationVertical]
            Gtk.toWidget boxLayout
        )
    |-> ( \(Button text) runner handleEvent -> do
            button <- Gtk.new Gtk.Button [#label Gtk.:= text]
            Gtk.on button #clicked (handleEvent Click)
            widget <- Gtk.toWidget button
            i <- nextId
            fontCss >>= addCssToWidget widget i
            pure widget
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
                generateWidget state = do

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
        local (\s -> s{gtkFontSize=f (gtkFontSize s)}) $ 
          runMarkup runner handleEvent markup
        )
    |-> (\(Set weight markup) runner handleEvent -> do
        local (\s -> s{gtkFontWeight = weight}) $ 
          runMarkup runner handleEvent markup
        )
    |-> (\(Set (style) markup) runner handleEvent -> do
        local (\s -> s{gtkFontStyle=style}) $ 
          runMarkup runner handleEvent markup
        )
    |-> (\(Set orientation markup) runner handleEvent -> do
      local (\s -> s{gtkOrientation=orientation}) $ 
        runMarkup runner handleEvent markup
      )
    |-> runnerFontColour


runnerFontColour :: AddToRunner (Set FontColour) IO (Gtk Gtk.Widget)
runnerFontColour = (\(Set colour markup) runner handleEvent -> do
      local (\s -> s{gtkFontColour = colour}) $ 
        runMarkup runner handleEvent markup
      )

fontCss :: Gtk T.Text
fontCss = foldl (<>) "" <$> sequenceA [ask (fontColour . gtkFontColour), ask (fontSize . gtkFontSize), ask (fontStyle . gtkFontStyle), ask (fontWeight . gtkFontWeight)]
  where 
    fontColour (FontColour colour) = "color:" <> T.pack (sRGB24show colour)  <> ";"
    fontSize size = "font-size:" <> T.pack (show size) <> "px;"
    fontStyle style = 
      let styleText = case style of
            RegularStyle -> "normal"
            ItalicStyle -> "italic"
      in "font-style:" <> styleText <> ";"
    fontWeight (FontWeight weight) = "font-weight:" <> T.pack (show weight) <> ";"


newtype Gtk a = Gtk (R.ReaderT GtkState IO a) deriving (Functor, Applicative, Monad, MonadIO)

ask :: (GtkState -> a) -> Gtk a
ask f = Gtk $ f <$> R.ask
  
local :: (GtkState -> GtkState) -> Gtk a -> Gtk a
local f (Gtk s) = Gtk $ R.local f s

nextId :: Gtk Int
nextId = do
  ref <- ask gtkId
  i <- liftIO $ atomicModifyIORef ref (\i -> (succ i, i))
  pure i 

withinIO :: (s -> Gtk a) -> Gtk (s -> IO a)
withinIO f = do
  state <- ask id
  pure $ \s -> do
    a <- runGtk state (f s)
    pure a

runGtk :: GtkState -> Gtk a -> IO a
runGtk state (Gtk s) = R.runReaderT s state

defaultGtkState :: IO GtkState
defaultGtkState = newIORef 0 <&> \ref -> GtkState Vertical (FontColour black) (FontWeight 400) RegularStyle  15 ref

data GtkState = GtkState
  { gtkOrientation :: Set Orientation,
    gtkFontColour :: Set FontColour,
    gtkFontWeight :: Set FontWeight,
    gtkFontStyle :: Set FontStyle,
    gtkFontSize :: Int,
    gtkId :: IORef Int
  }

-- | Quick setup for GTK to display a widget generated by 'runMarkup' in combination with 'gtkRunner'.
--
-- Example: main = basicGtkSetup "MyWindowName" $ runMarkup gtkRunner (\_ -> pure ()) myMarkup
basicGtkSetup :: T.Text -> Gtk Gtk.Widget -> IO ()
basicGtkSetup windowTitle widget = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window [#title Gtk.:= windowTitle]
  Gtk.on win #destroy Gtk.mainQuit
  gtkState <- defaultGtkState
  runGtk gtkState widget >>= #add win
  #showAll win
  Gtk.main

addCssToWidget :: MonadIO m => Gtk.Widget -> Int -> T.Text -> m ()
addCssToWidget widget i css = do
  let widgetName = "e" <> T.pack (show i)
  cssProvider <- Gtk.cssProviderNew
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.widgetSetName widget widgetName
  Gtk.cssProviderLoadFromData cssProvider $ T.encodeUtf8 ("#" <> widgetName <> "{" <> css <> "}")
  Gtk.styleContextAddProvider styleContext cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
