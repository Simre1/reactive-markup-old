module ReactiveMarkup.Elements.Basic
  ( Label,
    label,
    List,
    list,
    Button,
    button,
    DynamicState,
    dynamicState,
    DynamicMarkup,
    dynamicMarkup,
    Element (..),
  )
where

import Data.Colour
import qualified Data.Text as T
import ReactiveMarkup.Markup
import ReactiveMarkup.SimpleEvents

data Label (options :: [*]) deriving (Typeable)

data instance Element (Label options) elems e = Label (Options options e)

-- | Just a label element.
label :: Options options e -> Markup '[Label options] '[] e
label = toMarkup . Label

data List (options :: [*]) deriving (Typeable)

data instance Element (List options) elems e =
    List (Options options e) [SimpleMarkup elems e]

-- | Allows to combine multiple elements into one.
-- You can create an empty `MarkupBuilder` with `emptyMarkupBuilder` and add elements to it with `+->`.
list :: Options options e -> MarkupBuilder elems1 elems2 e -> Markup '[List options] (Merge elems1 elems2) e
list options = toMarkup . List options . getSimpleMarkups

data Button (options :: [*]) deriving (Typeable)

data instance Element (Button options) elems e = Button (Options options e)

-- | Button which emits the event `ButtonInfo`.
button :: Options options e -> Markup '[Button options] '[] e
button = toMarkup . Button

data DynamicState deriving (Typeable)

data instance Element DynamicState merged e
  = forall state innerEvent elems children.
    merged ~ Merge elems children =>
    DynamicState state (state -> innerEvent -> (Maybe state, Maybe e)) (Dynamic state -> Markup elems children innerEvent)

-- | Allows local state and event handling within `Markup`.
dynamicState ::
  -- | Initial state
  state ->
  -- | Handles an event by changing local state and/or emitting another event.
  (state -> innerEvent -> (Maybe state, Maybe outerEvent)) ->
  -- | Local state can be used to create `Markup`.
  (Dynamic state -> Markup elems1 elems2 innerEvent) ->
  Markup '[DynamicState] (Merge elems1 elems2) outerEvent
dynamicState initialState handleEvent markup = toMarkup $ DynamicState initialState handleEvent markup

data DynamicMarkup deriving (Typeable)

data instance Element DynamicMarkup merged e
  = forall state elems children.
    merged ~ Merge elems children =>
    DynamicMarkup (Dynamic state) (state -> Markup elems children e)

-- | Replaces the Markup whenever state changes.
dynamicMarkup :: Dynamic state -> (state -> Markup elems children e) -> Markup '[DynamicMarkup] (Merge elems children) e
dynamicMarkup dynamicMarkup markup = toMarkup $ DynamicMarkup dynamicMarkup markup
