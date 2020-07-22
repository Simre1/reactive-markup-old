module ReactiveMarkup.Elements.Layout
  ( FlowLayout,
    flowLayout,
    GridLayout,
    GridOptions (..),
    gridLayout,
    GridChild,
    gridChild,
    GridPosition (..),
    Element (..),
  )
where

import qualified Data.Text as T
import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Markup

data FlowLayout deriving (Typeable)

data instance Element FlowLayout merged e
  = forall elems children.
    merged ~ Merge elems children =>
    FlowLayout [Markup elems children e]

flowLayout :: MarkupBuilder elems children e -> Markup '[FlowLayout] (Merge elems children) e
flowLayout markupBuilder = toMarkup $ FlowLayout (getMarkups markupBuilder)

data GridLayout

data GridChild

data instance Element GridLayout elems e = GridLayout GridOptions [Markup '[GridChild] elems e]

data instance Element GridChild merged e
  = forall elems children.
    merged ~ (Merge elems children) =>
    GridChild GridPosition (Markup elems children e)

data GridPosition = GridPosition
  { gridChildColumn :: Int,
    gridChildRow :: Int,
    gridChildWidth :: Int,
    gridChildHeight :: Int
  }

data GridOptions = GridOptions
  { gridRows :: Int,
    gridColumns :: Int
  }

gridLayout :: GridOptions -> MarkupBuilder '[GridChild] children e -> Markup '[GridLayout] children e
gridLayout options = toMarkup . GridLayout options . getMarkups

gridChild :: GridPosition -> Markup elems children e -> Markup '[GridChild] (Merge elems children) e
gridChild position = toMarkup . GridChild position
