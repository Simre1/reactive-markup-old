module ReactiveMarkup.Elements.Basic
  ( Label,
    label,
    List,
    list,
    Box,
    box,
    Button,
    button,
    Window,
    window,
    DynamicState,
    dynamicState,
    DynamicStateIO,
    dynamicStateIO,
    DynamicMarkup,
    dynamicMarkup,
    HandleEvent,
    handleEvent,
    HandleEventIO,
    handleEventIO,
    Element (..),
  )
where

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
list :: Options options e -> [Markup elems1 elems2 e] -> Markup '[List options] (elems1 <+ elems2) e
list options = toMarkup . List options . fmap toSimpleMarkup

data Box (options :: [*]) deriving Typeable

data instance Element (Box options) elems e = Box (Options options e) (SimpleMarkup elems e)

box :: Options options e -> Markup elems children e -> Markup '[Box options] (elems <+ children) e
box options = toMarkup . Box options . toSimpleMarkup

data Button (options :: [*]) deriving (Typeable)

data instance Element (Button options) elems e = Button (Options options e)

-- | A simple button.
button :: Options options e -> Markup '[Button options] '[] e
button = toMarkup . Button

data Window (options :: [*]) deriving (Typeable)

data instance Element (Window options) elems e = Window (Options options e) (SimpleMarkup elems e)

-- | A window. 
window :: Options options e -> Markup elems children e -> Markup '[Window options] (elems <+ children) e
window options = toMarkup . Window options . toSimpleMarkup

data DynamicState deriving (Typeable)

data instance Element DynamicState elems e
  = forall state innerEvent.
    DynamicState state (state -> innerEvent -> (Maybe state, Maybe e)) (Dynamic state -> SimpleMarkup elems innerEvent)

-- | Allows local state and event handling within `Markup`.
dynamicState ::
  -- | Initial state
  state ->
  -- | Handles an event by changing local state and/or emitting another event.
  (state -> innerEvent -> (Maybe state, Maybe outerEvent)) ->
  -- | Local state can be used to create `Markup`.
  (Dynamic state -> Markup elems1 elems2 innerEvent) ->
  Markup '[DynamicState] (elems1 <+ elems2) outerEvent
dynamicState initialState handleEvent markup = toMarkup $ DynamicState initialState handleEvent (toSimpleMarkup . markup)

data DynamicStateIO deriving (Typeable)

data instance Element DynamicStateIO elems e
  = forall state innerEvent.
    DynamicStateIO state (state -> innerEvent -> IO (Maybe state, Maybe e)) (Dynamic state -> SimpleMarkup elems innerEvent)

-- | Allows local state and event handling within `Markup`.
dynamicStateIO ::
  -- | Initial state
  state ->
  -- | Handles an event by changing local state and/or emitting another event.
  (state -> innerEvent -> IO (Maybe state, Maybe outerEvent)) ->
  -- | Local state can be used to create `Markup`.
  (Dynamic state -> Markup elems1 elems2 innerEvent) ->
  Markup '[DynamicStateIO] (elems1 <+ elems2) outerEvent
dynamicStateIO initialState handleEvent = toMarkup . DynamicStateIO initialState handleEvent . (toSimpleMarkup.)

data DynamicMarkup deriving (Typeable)

data instance Element DynamicMarkup elems e
  = forall state.
    DynamicMarkup (Dynamic state) (state -> SimpleMarkup elems e)

-- | Replaces the Markup whenever state changes.
dynamicMarkup :: Dynamic state -> (state -> Markup elems children e) -> Markup '[DynamicMarkup] (elems <+ children) e
dynamicMarkup dynamicMarkup markup = toMarkup $ DynamicMarkup dynamicMarkup (toSimpleMarkup . markup)

data HandleEvent deriving Typeable

data instance Element HandleEvent elems e = forall innerEvent.
  HandleEvent (innerEvent -> Maybe e) (SimpleMarkup elems innerEvent)

handleEvent :: (innerEvent -> Maybe e) -> Markup elems children innerEvent -> Markup '[HandleEvent] (elems <+ children) e
handleEvent f = toMarkup . HandleEvent f . toSimpleMarkup

data HandleEventIO deriving Typeable

data instance Element HandleEventIO elems e = forall innerEvent.
  HandleEventIO (innerEvent -> IO (Maybe e)) (SimpleMarkup elems innerEvent)

handleEventIO :: (innerEvent -> IO (Maybe e)) -> Markup elems children innerEvent -> Markup '[HandleEventIO] (elems <+ children) e
handleEventIO f = toMarkup . HandleEventIO f . toSimpleMarkup
