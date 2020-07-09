module ReactiveMarkup.Runners.Gtk where

import qualified GI.Gtk as Gtk
import Data.IORef

import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Dynamic
import ReactiveMarkup.SimpleEvents


-- | Basic `Runner` for GTK 3. It is definitely not optimal, but is sufficient as demontration.
gtkRunner :: Runner [Text, List, Button, LocalState] IO (Gtk.Widget)
gtkRunner = emptyRunner
  |-> (\(Text t) _ _ -> do
    label <- Gtk.new Gtk.Label [#label Gtk.:= t]
    Gtk.toWidget label
    )
  |-> (\(List markups) runner handleEvent -> do 
        boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
        sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> markups
        Gtk.toWidget boxLayout
      )
  |-> (\(Button) _ handleEvent -> do
        button <- Gtk.new Gtk.Button [#label Gtk.:="Button"]
        Gtk.on button #clicked (handleEvent Click)
        Gtk.toWidget button
      )
  |-> (\(LocalState state handleInnerEvent generateMarkup) runner handleOuterEvent -> do
      stateRef <- newIORef state
      cleanUpRef <- newIORef (pure ())
      boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
      let 
        replaceWidget widget = do
          cleanUp <- readIORef cleanUpRef
          cleanUp
          writeIORef cleanUpRef (#remove boxLayout widget)
          #add boxLayout widget
          #showAll widget
        handleChildrenEvent e = do
          currentState <- readIORef stateRef
          let (newState, outerEvent) = handleInnerEvent currentState e 
          case outerEvent of
            Nothing -> pure ()
            Just e -> handleOuterEvent e
          case newState of
            Nothing -> pure ()
            Just newState -> do
              writeIORef stateRef newState
              newWidget <- runMarkup runner handleChildrenEvent (generateMarkup newState)
              replaceWidget $ newWidget
      widget <- runMarkup runner handleChildrenEvent (generateMarkup state)
      replaceWidget $ widget
      Gtk.toWidget boxLayout
    )
