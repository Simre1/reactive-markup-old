module Runners.Gtk where

import qualified GI.Gtk as Gtk
import Data.IORef

import Markup
import BasicElements

-- | Rather inefficient runner, but functions as a minimal implementation for now.
gtkRunner :: Runner [Text, List, Button, Local] (IO Gtk.Widget)
gtkRunner = emptyRunner
  |-> (\(Text t) _ _ -> Gtk.new Gtk.Label [#label Gtk.:= t] >>= Gtk.toWidget)
  |-> (\(List markups) runner handleEvent -> do 
        boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
        sequenceA $ (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) <$> markups
        Gtk.toWidget boxLayout
      )
  |-> (\(Button f) _ handleEvent -> do
        button <- Gtk.new Gtk.Button [#label Gtk.:="Button"]
        Gtk.on button #clicked (handleEvent $ f Click)
        Gtk.toWidget button
      )
  |-> (\(Local state handleInnerEvent generateMarkup) runner handleOuterEvent -> do
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
          writeIORef stateRef newState
          newWidget <- runMarkup runner handleChildrenEvent (generateMarkup newState)
          replaceWidget $ newWidget
      widget <- runMarkup runner handleChildrenEvent (generateMarkup state)
      replaceWidget $ widget
      Gtk.toWidget boxLayout
    )
