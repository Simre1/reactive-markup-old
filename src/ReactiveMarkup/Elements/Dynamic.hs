-- Currently in Review phase, since Dynamic might not be needed.

module ReactiveMarkup.Elements.Dynamic 
  ( DynamicText
  , dynamicText
  , DynamicList
  , dynamicList
  , DynamicLocalState
  , dynamicLocalState
  , Element(..)
  ) 
  where

import ReactiveMarkup.SimpleEvents
import ReactiveMarkup.Markup

import qualified Data.Text as T

data DynamicText deriving Typeable
data instance Element DynamicText elems e = DynamicText (Dynamic T.Text)

data DynamicList deriving Typeable
data instance Element DynamicList merged e = forall direct transitive. merged ~ (Merge direct transitive) => DynamicList (Dynamic [Markup direct transitive e])

data DynamicLocalState deriving Typeable
data instance Element DynamicLocalState merged e = forall state innerEvent elems children. merged ~ Merge elems children =>
  DynamicLocalState state (state -> innerEvent -> (Maybe state, Maybe e)) (Dynamic state -> Markup elems children innerEvent)

-- | Text element with changing text.
dynamicText :: Dynamic T.Text -> Markup '[DynamicText] '[] e
dynamicText dText = toMarkup $ DynamicText dText

-- | Allows to combine multiple elements into one. 
-- You can create an empty `MarkupBuilder` with `emptyMarkupBuilder` and add elements to it with `+->`.
dynamicList :: Dynamic (MarkupBuilder elems1 elems2 e) -> Markup '[DynamicList] (Merge elems1 elems2) e
dynamicList ls = toMarkup $ DynamicList (getMarkups <$> ls)

-- | Allows local state and event handling within `Markup`.
dynamicLocalState :: 
  state -- ^ Initial state
  -> (state -> innerEvent -> (Maybe state, Maybe outerEvent)) -- ^ Handles an event by changing local state and/or emitting another event.
  -> (Dynamic state -> Markup elems1 elems2 innerEvent) -- ^ Local state can be used to create `Markup`.
  -> Markup '[DynamicLocalState] (Merge elems1 elems2) outerEvent
dynamicLocalState initialState handleEvent markup = toMarkup $ DynamicLocalState initialState handleEvent $ toSimpleMarkup . markup
