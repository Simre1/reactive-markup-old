{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.GridLayout where

import qualified Data.Text as T

import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Layout
import ReactiveMarkup.Markup
import ReactiveMarkup.Runners.Gtk


grid :: Markup '[Set FontSize] '[GridLayout, Label] e
grid = fontSizePx 30 %-> gridLayout (GridOptions 4 4) (emptyMarkupBuilder
  +-> gridChild (GridPosition 1 1 1 1) (label "[0,0]")
  +-> gridChild (GridPosition 1 3 3 1) (label "[0,2] -> [2,2]")
  +-> gridChild (GridPosition 2 1 2 1) (label "[1,0] -> [2,0]")
  +-> gridChild (GridPosition 2 2 1 1) (label "[2,1]")
  +-> gridChild (GridPosition 1 0 1 1) (label "Column")
  +-> gridChild (GridPosition 2 0 1 1) (label "Column")
  +-> gridChild (GridPosition 3 0 1 1) (label "Column")
  +-> gridChild (GridPosition 0 1 1 1) (label "Row")
  +-> gridChild (GridPosition 0 2 1 1) (label "Row")
  +-> gridChild (GridPosition 0 3 1 1) (label "Row")
  )
