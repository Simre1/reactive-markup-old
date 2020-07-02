module BasicElements 
  ( Text
  , text
  , List
  , list
  , Button
  , button
  , ButtonInfo(..)
  , LocalState
  , localState
  , Element(..)
  )
  where

import Markup

import qualified Data.Text as T

data Text deriving Typeable
data instance Element Text elems e = Text T.Text

data List deriving Typeable
data instance Element List merged e = forall direct transitive. merged ~ (Merge direct transitive) => List [Markup direct transitive e]

data LocalState deriving Typeable
data instance Element LocalState elems e = forall state innerEvent. LocalState state (state -> innerEvent -> (state, Maybe e)) (state -> SimpleMarkup elems innerEvent)

data Button deriving Typeable
data instance Element Button elems e = Button (ButtonInfo -> e)
data ButtonInfo = Click

instance Functor (Element Text elems) where
  fmap _ (Text t) = (Text t)

instance Functor (Element List elems) where
  fmap f (List markups) = List $ fmap (fmap f) markups

instance Functor (Element LocalState elems) where
  fmap f (LocalState s handleEvent markup) = LocalState s (\s i -> (\(ns,e) -> (ns, f <$> e)) $ handleEvent s i) markup

instance Functor (Element Button elems) where
  fmap f (Button g) = Button (f . g)

-- | Just a text element.
text :: T.Text -> Markup '[Text] '[] e
text = toMarkup . Text

-- | Allows to combine multiple elements into one. 
-- You can create an empty `MarkupBuilder` with `emptyMarkupBuilder` and add elements to it with `+->`.
list :: MarkupBuilder elems1 elems2 e -> Markup '[List] (Merge elems1 elems2) e
list ls = toMarkup $ List (getMarkups ls)

-- | Allows local state and event handling within `Markup`.
localState :: 
  state -- ^ Initial state
  -> (state -> innerEvent -> (state, Maybe outerEvent)) -- ^ Handles an event by changing local state and/or emitting another event.
  -> (state -> Markup elems1 elems2 innerEvent) -- ^ Local state can be used to create `Markup`.
  -> Markup '[LocalState] (Merge elems1 elems2) outerEvent
localState initialState handleEvent markup = toMarkup $ LocalState initialState handleEvent $ toSimpleMarkup . markup

-- | Button which emits the event `ButtonInfo`.
button :: Markup '[Button] '[] ButtonInfo
button = toMarkup $ Button id