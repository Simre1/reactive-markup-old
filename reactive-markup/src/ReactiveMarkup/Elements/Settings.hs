module ReactiveMarkup.Elements.Settings
  ( FontSize
  , fontSizePx
  , greaterFont
  , smallerFont
  , Orientation
  , horizontal
  , vertical
  , FontStyle
  , italicStyle
  , regularStyle
  , FontWeight
  , bold
  , regularWeight
  , FontColour
  , fontColour
  , Set(..)
  , Element(..)
  , Setting
  , makeSetting
  , (%%)
  , (%->)
  , ) 
  where

import ReactiveMarkup.Markup

import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)
import Data.Colour.SRGB (Colour)

data family Set setting

data instance Element (Set setting) elems e = Set (Set setting) (SimpleMarkup elems e)  deriving Typeable

-- TODO: Remove unsafeCoerce. There should be a type-safe solution with a type class.
applySettings :: Setting (Set setting:rest) -> Markup elems children e -> Markup '[Set setting] (Merge (Unique rest) (Merge elems children)) e
applySettings (SingleSet setting) markup = toMarkup $ Set setting $ toSimpleMarkup markup
applySettings (NextSet setting otherSettings) markup = toMarkup $ Set setting $ unsafeCoerce $ 
  toSimpleMarkup $ applySettings otherSettings markup

data Setting settings where
  SingleSet :: Typeable (Set setting) => Set setting -> Setting '[Set setting]
  NextSet :: Typeable (Set setting) => (Set (setting)) -> Setting (Set s:ss) -> Setting (Set setting:Set s:ss)

(%%) :: Typeable s => Setting '[Set s] -> Setting (Set s1:s2) -> Setting (Set s:Set s1:s2)
(%%) (SingleSet setting) = NextSet setting

infix 4 %%

(%->) :: Setting (Set setting : rest) -> Markup elems children e -> Markup '[Set setting] (Merge (Unique rest) (Merge elems children)) e
(%->) = applySettings

infix 3 %->

makeSetting :: Typeable setting => Set setting -> Setting '[Set setting]
makeSetting = SingleSet

data FontSize deriving Typeable
data instance Set FontSize = FontSize (Int -> Int) deriving Typeable

data Orientation deriving Typeable  
data instance Set Orientation = Horizontal | Vertical deriving (Eq, Show, Typeable)

data FontStyle deriving Typeable
data instance Set FontStyle = ItalicStyle | RegularStyle deriving Typeable

data FontWeight deriving Typeable
data instance Set FontWeight = FontWeight Int deriving Typeable

data FontColour deriving Typeable 
data instance Set FontColour = FontColour (Colour Double) deriving Typeable

smallerFont :: Setting '[Set FontSize]
smallerFont = makeSetting $ FontSize (\x -> round $ fromIntegral x * 0.7)

greaterFont :: Setting '[Set FontSize]
greaterFont = makeSetting $ FontSize (\x -> round $ fromIntegral x * 1.3)

fontSizePx :: Int -> Setting '[Set FontSize]
fontSizePx px = makeSetting $ FontSize (const px)

horizontal :: Setting '[Set Orientation]
horizontal = makeSetting $ Horizontal

vertical :: Setting '[Set Orientation]
vertical = makeSetting $ Vertical

italicStyle :: Setting '[Set FontStyle]
italicStyle = makeSetting $ ItalicStyle

regularStyle :: Setting '[Set FontStyle]
regularStyle = makeSetting $ RegularStyle

bold :: Setting '[Set FontWeight]
bold = makeSetting $ FontWeight 700

regularWeight :: Setting '[Set FontWeight]
regularWeight = makeSetting $ FontWeight 400

fontColour :: Colour Double -> Setting '[Set FontColour]
fontColour = makeSetting . FontColour