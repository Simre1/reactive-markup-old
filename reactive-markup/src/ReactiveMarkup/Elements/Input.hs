module ReactiveMarkup.Elements.Input where

import qualified Data.Text as T
import ReactiveMarkup.Elements.Settings
import ReactiveMarkup.Markup

data TextInput deriving (Typeable)

data instance Element TextInput elems e = TextInput

textInput :: Markup '[TextInput] '[] e
textInput = toMarkup TextInput
