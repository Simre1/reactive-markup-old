module ReactiveMarkup.Elements.Menu where

import qualified Data.Text as T
import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.SimpleEvents

data Menu deriving Typeable

data instance Element Menu elems e = Menu [SimpleMarkup elems e]

menu :: [Markup elems children e] -> Markup '[Menu] (elems <+ children) e
menu = toMarkup . Menu . fmap toSimpleMarkup