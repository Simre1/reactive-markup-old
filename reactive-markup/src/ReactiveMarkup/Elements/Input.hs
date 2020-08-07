module ReactiveMarkup.Elements.Input where

import qualified Data.Text as T
import ReactiveMarkup.Markup
import Data.Word (Word32)

data TextInput (options :: [*]) deriving (Typeable)

data instance Element (TextInput options) elems e = TextInput (Options options e)

textInput :: Options options e -> Markup '[TextInput options] '[] e
textInput = toMarkup . TextInput

data Modifier = ModShift | ModControl | ModAlt | ModSuper deriving (Eq, Show)

data HotKey deriving (Typeable)

data instance Element HotKey elems e = HotKey (Key -> [Modifier] -> Maybe e) (SimpleMarkup elems e)

data Key
  = KeyQ
  | KeyW
  | KeyE
  | KeyR
  | KeyT
  | KeyZ
  | KeyU
  | KeyI
  | KeyO
  | KeyP
  | KeyA
  | KeyS
  | KeyD
  | KeyF
  | KeyJ
  | KeyK
  | KeyL
  | KeyY
  | KeyX
  | KeyC
  | KeyV
  | KeyB
  | KeyN
  | KeyM
  | KeyEnter
  | KeySpace
  deriving (Eq, Show)

hotKeyFunction :: (Key -> [Modifier] -> Maybe e) -> Markup elems children e -> Markup '[HotKey] (elems <+ children) e
hotKeyFunction f child = toMarkup $ HotKey f $ toSimpleMarkup child

hotKey :: Key -> [Modifier] -> e -> Markup elems children e -> Markup '[HotKey] (elems <+ children) e
hotKey givenKey givenModifiers event = hotKeyFunction $ \key modifiers ->
  if (givenKey == key) && all (`elem` modifiers) givenModifiers
    then Just event
    else Nothing