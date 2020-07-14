{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Runners.Gtk

main :: IO ()
main = basicGtkSetup "Example" $ runMarkup gtkRunner (\_ -> pure ()) myMarkup

myMarkup :: SimpleMarkup [Label, List, DynamicState, DynamicMarkup, Button] Void
myMarkup = expandMarkup $ list mempty $ emptyMarkupBuilder
  +-> label mempty "Some text"
  +-> label #italic "Italic text"
  +-> list mempty (emptyMarkupBuilder +-> label #bold "Bold text")
  +-> dynamicState 0 (\i _ -> (Just $ succ i, Nothing)) 
        (flip dynamicMarkup $ \i -> list mempty $ emptyMarkupBuilder 
          +-> button "Increase"
          +-> label (#bold <> #italic) (T.pack $ show i)
        )