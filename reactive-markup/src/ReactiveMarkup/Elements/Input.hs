module ReactiveMarkup.Elements.Input where

import qualified Data.Text as T

import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Settings

data TextInput deriving Typeable
data instance Element TextInput elems e = TextInput

textInput :: Markup '[TextInput] '[] e
textInput = toMarkup TextInput