module ReactiveMarkup.Elements.Layout
  ( FlowLayout,
    flowLayout,
    GridLayout,
    gridLayout,
    GridChild,
    gridChild,
    GridPosition (..),
    Notebook,
    notebook,
    Element (..),
  )
where

import Control.Arrow (Arrow (second))
import qualified Data.Text as T
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Markup

data FlowLayout (options :: [*]) deriving (Typeable)

data instance Element (FlowLayout options) elems e
  = FlowLayout (Options options e) [SimpleMarkup elems e]

flowLayout :: Options options e -> [Markup elems children e] -> Markup '[FlowLayout options] (elems <+ children) e
flowLayout options markups = toMarkup $ FlowLayout options (toSimpleMarkup <$> markups)

data GridLayout (options :: [*]) deriving (Typeable)

data GridChild deriving (Typeable)

data instance Element (GridLayout options) elems e = GridLayout (Options options e) [Markup '[GridChild] elems e]

data instance Element GridChild elems e
  = GridChild GridPosition (SimpleMarkup elems e)

data GridPosition = GridPosition
  { gridChildColumn :: Int,
    gridChildRow :: Int,
    gridChildWidth :: Int,
    gridChildHeight :: Int
  }

gridLayout :: Options options e -> [Markup '[GridChild] children e] -> Markup '[GridLayout options] children e
gridLayout options = toMarkup . GridLayout options

gridChild :: GridPosition -> Markup elems children e -> Markup '[GridChild] (elems <+ children) e
gridChild position = toMarkup . GridChild position . toSimpleMarkup

data Notebook deriving (Typeable)

data instance Element Notebook merged e
  = forall elems options.
    ( merged ~ (elems <+ '[Label options]),
    SubList '[Label options] merged,
    SubList elems merged
    ) =>
    Notebook [(Markup '[Label options] '[] e, SimpleMarkup elems e)]

notebook ::
  forall merged options e elems children.
  ( merged ~ ((elems <+ children) <+ '[Label options]),
    SubList '[Label options] merged,
    SubList (elems <+ children) merged
  ) =>
  [(Markup '[Label options] '[] e, Markup elems children e)] ->
  Markup '[Notebook] merged e
notebook = toMarkup . Notebook . fmap (second $ toSimpleMarkup)