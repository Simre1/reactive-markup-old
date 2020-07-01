module Main where

import qualified GI.Gtk as Gtk

import Markup
import BasicElements
import Runners.Gtk
import qualified Data.Text as T

main :: IO ()
main = do
  Gtk.init Nothing

  win <- Gtk.new Gtk.Window [ #title Gtk.:= "Example" ]

  Gtk.on win #destroy Gtk.mainQuit

  widget <- runMarkup gtkRunner (\_ -> pure ()) myMarkup

  #add win widget

  #showAll win

  Gtk.main

myMarkup :: SimpleMarkup [Text, List, Local, Button] Void
myMarkup = expandMarkup $ list $ emptyMarkupBuilder
  +-> text "First element"
  +-> text "Second element"
  +-> list (emptyMarkupBuilder +-> text "Element sub list")
  +-> local (2,1) 
    (\(i,sum) _ -> ( (succ i, sum * i), Nothing)) 
    (\(_,sum) -> list $ emptyMarkupBuilder 
      +-> button +-> text (T.pack $ show sum)
      +-> local 0 (\i _ -> (succ i, Nothing)) 
        (\i -> list $ emptyMarkupBuilder 
          +-> button 
          +-> text (T.pack $ show i)
        )
    )
