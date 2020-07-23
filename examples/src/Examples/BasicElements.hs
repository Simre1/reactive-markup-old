{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.BasicElements where

import Data.Colour
import Data.Colour.Names
import qualified Data.Text as T
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Options
import ReactiveMarkup.Markup
import ReactiveMarkup.Runners.Gtk

basicElements :: SimpleMarkup '[Label '[Text, FontWeight, FontStyle, FontSize, FontColour], List '[], DynamicState, DynamicMarkup, Button '[Click, Text]] e
basicElements = expandMarkup $
    list
      none
      ( emptyMarkupBuilder
          +-> (label (text "Some text"))
          +-> label (italicStyle %% text "Italic text")
          +-> list none (emptyMarkupBuilder +-> label (bold %% text "Bold text") +-> label (bold %% text "Another bold text"))
          +-> dynamicState
            0
            (\i _ -> (Just $ succ i, Nothing))
            ( flip dynamicMarkup $ \i ->
                list
                  none
                  ( emptyMarkupBuilder
                      +-> button (onClick () %% text "Change Colour")
                      +-> label (fontSizePx 20 %% fontColour (rainbowColour i) %% text "Colourful!")
                  )
            )
      )

rainbowColour :: Int -> Colour Double
rainbowColour i = blend factor3 (blend factor2 red yellow) (blend factor2 (blend factor1 red yellow) blue)
  where
    factor1 = sin (0.35 * fromIntegral i)
    factor2 = sin (0.2 * fromIntegral i)
    factor3 = 0.5 + sin (0.25 * fromIntegral i) / 4
