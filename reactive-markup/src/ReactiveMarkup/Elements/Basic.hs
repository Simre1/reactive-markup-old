module ReactiveMarkup.Elements.Basic 
  ( Label
  , label
  , List
  , list
  , Button
  , button
  , ButtonInfo(..)
  , DynamicState
  , dynamicState
  , DynamicMarkup
  , dynamicMarkup
  , Element(..)
  )
  where

import qualified Data.Text as T
import Data.Colour

import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents

data Label deriving Typeable
data instance Element Label elems e = Label T.Text
-- | Just a label element.
label :: T.Text -> Markup '[Label] '[] e
label text = toMarkup $ Label text

data List deriving Typeable
data instance Element List merged e = forall direct transitive. merged ~ (Merge direct transitive) => List [Markup direct transitive e]

-- | Allows to combine multiple elements into one. 
-- You can create an empty `MarkupBuilder` with `emptyMarkupBuilder` and add elements to it with `+->`.
list :: MarkupBuilder elems1 elems2 e -> Markup '[List] (Merge elems1 elems2) e
list ls = toMarkup $ List (getMarkups ls)

data Button deriving Typeable
data instance Element Button elems e = e ~ ButtonInfo => Button T.Text
data ButtonInfo = Click

-- | Button which emits the event `ButtonInfo`.
button :: T.Text -> Markup '[Button] '[] ButtonInfo
button = toMarkup . Button

data DynamicState deriving Typeable
data instance Element DynamicState merged e = forall state innerEvent elems children. merged ~ Merge elems children =>
  DynamicState state (state -> innerEvent -> (Maybe state, Maybe e)) (Dynamic state -> Markup elems children innerEvent)

-- | Allows local state and event handling within `Markup`.
dynamicState :: 
  state -- ^ Initial state
  -> (state -> innerEvent -> (Maybe state, Maybe outerEvent)) -- ^ Handles an event by changing local state and/or emitting another event.
  -> (Dynamic state -> Markup elems1 elems2 innerEvent) -- ^ Local state can be used to create `Markup`.
  -> Markup '[DynamicState] (Merge elems1 elems2) outerEvent
dynamicState initialState handleEvent markup = toMarkup $ DynamicState initialState handleEvent markup

data DynamicMarkup deriving Typeable
data instance Element DynamicMarkup merged e = forall state elems children. merged ~ Merge elems children =>
  DynamicMarkup (Dynamic state) (state -> Markup elems children e)

-- | Replaces the Markup whenever state changes.
dynamicMarkup :: Dynamic state -> (state -> Markup elems children e) -> Markup '[DynamicMarkup] (Merge elems children) e
dynamicMarkup dynamicMarkup markup = toMarkup $ DynamicMarkup dynamicMarkup markup