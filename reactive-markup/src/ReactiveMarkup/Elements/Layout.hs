module ReactiveMarkup.Elements.Layout
  ( FlowLayout,
    flowLayout,
    GridLayout,
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

flowLayout :: Options options e -> [Markup elems children e] -> Markup '[FlowLayout options] (elems <+ children) e
flowLayout options markups = toMarkup $ FlowLayout options (toSimpleMarkup <$> markups)

data GridLayout (options :: [*]) deriving Typeable

data GridChild deriving Typeable

data instance Element (GridLayout options) elems e = GridLayout (Options options e) [Markup '[GridChild] elems e]

data instance Element GridChild elems e =
    GridChild GridPosition (SimpleMarkup elems e)

data GridPosition = GridPosition
  { gridChildColumn :: Int,
    gridChildRow :: Int,
    gridChildWidth :: Int,
    gridChildHeight :: Int
  }

gridLayout :: Options options e -> [Markup '[GridChild] children e] -> Markup '[GridLayout options] children e
gridLayout options = toMarkup . GridLayout options

gridLayout' :: Options options e -> MarkupBuilder '[GridChild] children e -> Markup '[GridLayout options] children e
gridLayout' options = gridLayout options . getMarkups

gridChild :: GridPosition -> Markup elems children e -> Markup '[GridChild] (elems <+ children) e
gridChild position = toMarkup . GridChild position . toSimpleMarkup
