module ReactiveMarkup.Elements.Menu where

import qualified Data.Text as T
import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.SimpleEvents

data Menu deriving Typeable

data MenuChild deriving Typeable

data instance Element Menu elems e = Menu (Markup '[MenuChild] elems e)

data instance Element MenuChild elems e = MenuChild (Markup '[Button '[]] '[] e)