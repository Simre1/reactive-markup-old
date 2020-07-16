module ReactiveMarkup.Elements.Layout where

import qualified Data.Text as T
import GHC.OverloadedLabels (IsLabel (..))
import ReactiveMarkup.Elements.Settings

import ReactiveMarkup.Markup
data FlowLayout deriving (Typeable)

-- data instance Element FlowLayout elems e = FlowLayout SFlowLayout [SimpleMarkup elems e]

-- data SFlowLayout = SFlowLayout
--   { flowLayoutOrientation :: Orientation,
--     flowLayoutSameChildrenSize :: Bool
--   }

-- flowLayout :: Setting SFlowLayout -> MarkupBuilder elems children e -> Markup '[FlowLayout] (Merge elems children) e
-- flowLayout settings markupBuilder = toMarkup $ FlowLayout (applySetting initial settings) (getSimpleMarkups markupBuilder)
--   where
--     initial = SFlowLayout Horizontal False
