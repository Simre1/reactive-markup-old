module ReactiveMarkup.Elements.Basic 
  ( Label
  , LabelStyle(..)
  , label
  , List
  , ListStyle(..)
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

import ReactiveMarkup.Markup

import ReactiveMarkup.Elements.Settings

import Data.Function ((&))
import qualified Data.Text as T
import GHC.OverloadedLabels (IsLabel(..))
import ReactiveMarkup.SimpleEvents

data Label deriving Typeable
data instance Element Label elems e = Label LabelStyle T.Text 

data LabelStyle = LabelStyle
  { labelGetBold :: Bold
  , labelGetItalic :: Italic
  , labelGetSize :: Size
  }

-- | Just a label element.
label :: Setting LabelStyle -> T.Text -> Markup '[Label] '[] e
label style text = toMarkup $ Label (applySetting initial style) text
  where initial = LabelStyle NotBold NotItalic Regular

labelItalic :: Setting LabelStyle
labelItalic = makeSetting $ \s -> s{labelGetItalic = Italic} 

labelBold :: Setting LabelStyle
labelBold = makeSetting $ \s -> s{labelGetBold = Bold} 

labelSize :: Size -> Setting LabelStyle
labelSize size = makeSetting $ \s -> s{labelGetSize = size}

instance IsLabel "bold" (Setting LabelStyle) where
  fromLabel = labelBold

instance IsLabel "italic" (Setting LabelStyle) where
  fromLabel = labelItalic

instance IsLabel "size" (Size -> Setting LabelStyle) where
  fromLabel = labelSize

data List deriving Typeable
data instance Element List merged e = forall direct transitive. merged ~ (Merge direct transitive) => List ListStyle [Markup direct transitive e]

data ListStyle = ListStyle
  { listGetOrientation :: Orientation
  }

-- | Allows to combine multiple elements into one. 
-- You can create an empty `MarkupBuilder` with `emptyMarkupBuilder` and add elements to it with `+->`.
list :: Setting ListStyle -> MarkupBuilder elems1 elems2 e -> Markup '[List] (Merge elems1 elems2) e
list setting ls = toMarkup $ List (applySetting initial setting) (getMarkups ls)
  where initial = ListStyle Vertical

listHorizontal :: Setting ListStyle
listHorizontal = makeSetting $ \s -> s{listGetOrientation=Horizontal}

listVertical :: Setting ListStyle
listVertical = makeSetting $ \s -> s{listGetOrientation=Vertical}

instance IsLabel "horizontal" (Setting ListStyle) where
  fromLabel = listHorizontal

instance IsLabel "vertical" (Setting ListStyle) where
  fromLabel = listVertical

data Button deriving Typeable
data instance Element Button elems ButtonInfo = Button T.Text
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