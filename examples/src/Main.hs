{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Colour
import Data.Colour.Names

import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Markup
import ReactiveMarkup.Runners.Gtk

main :: IO ()
main = basicGtkSetup "Example" (\_ -> pure ()) myMarkup

myMarkup :: SimpleMarkup '[Set FontColour, Set FontSize, List, Label, Set FontStyle, Set FontWeight, DynamicState, DynamicMarkup, Button] e
myMarkup = expandMarkup $ fontColour white %% fontSizePx 15 %-> list (emptyMarkupBuilder
  +-> (label "Some text")
  +-> italicStyle %-> label "Italic text"
  +-> bold %-> list (emptyMarkupBuilder +-> label "Bold text" +-> label "Another bold text")
  +-> greaterFont %-> dynamicState 0 (\i _ -> (Just $ succ i, Nothing))
        (flip dynamicMarkup $ \i -> fontColour (rainbowColour i) %-> list (emptyMarkupBuilder 
          +-> button "Change Colour"
          +-> bold %-> label "Colourful!"
        )))

rainbowColour :: Int -> Colour Double
rainbowColour i = blend factor3 (blend factor2 red yellow) (blend factor2 (blend factor1 red yellow) blue)
  where
    factor1 = sin (0.35 * fromIntegral i)
    factor2 = sin (0.2 * fromIntegral i)
    factor3 = 0.5 + sin (0.25 * fromIntegral i) / 4