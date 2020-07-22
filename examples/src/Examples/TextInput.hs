{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.TextInput where

import qualified Data.Text as T

import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Input
import ReactiveMarkup.Markup

textInputExample :: Markup '[GeneralOptions '[FontSize]] '[List, Label, DynamicState, SpecificOptions TextInput '[TextChange], DynamicMarkup] e
textInputExample = fontSizePx 30 %-> list (emptyMarkupBuilder
  +-> label "Write some Text"
  +-> dynamicState "" (\_ t -> (Just t, Nothing)) (\text -> list $ emptyMarkupBuilder
    +-> onTextChange id %-> textInput
    +-> dynamicMarkup text (\t -> label t)
  )
  )