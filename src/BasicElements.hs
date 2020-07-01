module BasicElements where

import Markup

import qualified Data.Text as T

data Text deriving Typeable
data List deriving Typeable
data Local deriving Typeable
data Button deriving Typeable

data instance Element Text elems e = Text T.Text
data instance Element List merged e = forall direct transitive. merged ~ (Merge direct transitive) => List [Markup direct transitive e]
data instance Element Local elems e = forall state innerEvent. Local state (state -> innerEvent -> (state, Maybe e)) (state -> SimpleMarkup elems innerEvent)

data ButtonInfo = Click

data instance Element Button elems e = Button (ButtonInfo -> e)

instance Functor (Element Text elems) where
  fmap _ (Text t) = (Text t)

instance Functor (Element List elems) where
  fmap f (List markups) = List $ fmap (fmap f) markups


instance Functor (Element Local elems) where
  fmap f (Local s handleEvent markup) = Local s (\s i -> (\(ns,e) -> (ns, f <$> e)) $ handleEvent s i) markup

instance Functor (Element Button elems) where
  fmap f (Button g) = Button (f . g)


text :: T.Text -> Markup '[Text] '[] e
text = toMarkup . Text

list :: MarkupBuilder elems1 elems2 e -> Markup '[List] (Merge elems1 elems2) e
list ls = toMarkup $ List (getMarkups ls)

local :: 
  state -> (state -> innerEvent -> (state, Maybe outerEvent)) -> 
  (state -> Markup elems1 elems2 innerEvent) -> Markup '[Local] (Merge elems1 elems2) outerEvent
local initialState handleEvent markup = toMarkup $ Local initialState handleEvent $ toSimpleMarkup . markup

button :: Markup '[Button] '[] ButtonInfo
button = toMarkup $ Button id