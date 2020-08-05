module ReactiveMarkup.Elements.Layout
  ( FlowLayout,
    flowLayout,
    GridLayout,
    GridOptions (..),
    gridLayout,
    gridLayout',
    GridChild,
    gridChild,
    GridPosition (..),
    Element (..),
  )
where

import qualified Data.Text as T
import ReactiveMarkup.Markup

data FlowLayout (options :: [*]) deriving (Typeable)

data instance Element (FlowLayout options) elems e =
    FlowLayout (Options options e) [SimpleMarkup elems e]

flowLayout :: Options options e -> [Markup elems children e] -> Markup '[FlowLayout options] (Merge elems children) e
flowLayout options markups = toMarkup $ FlowLayout options (toSimpleMarkup <$> markups)

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

gridLayout :: GridOptions -> [Markup '[GridChild] children e] -> Markup '[GridLayout] children e
gridLayout options = toMarkup . GridLayout options

gridLayout' :: GridOptions -> MarkupBuilder '[GridChild] children e -> Markup '[GridLayout] children e
gridLayout' options = gridLayout options . getMarkups

gridChild :: GridPosition -> Markup elems children e -> Markup '[GridChild] (Merge elems children) e
gridChild position = toMarkup . GridChild position
