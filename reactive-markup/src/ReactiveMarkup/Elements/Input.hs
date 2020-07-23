module ReactiveMarkup.Elements.Input where

import qualified Data.Text as T
import ReactiveMarkup.Markup

data TextInput (options :: [*]) deriving (Typeable)

data instance Element (TextInput options) elems e = TextInput (Options options e)

textInput :: Options options e -> Markup '[TextInput options] '[] e
textInput = toMarkup . TextInput
