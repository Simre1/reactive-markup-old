{-# LANGUAGE UndecidableInstances #-}

module ReactiveMarkup.Elements.Options
where

import Data.Colour.SRGB (Colour)
import qualified Data.Text as T
import Data.Typeable
import ReactiveMarkup.Markup
import Unsafe.Coerce (unsafeCoerce)

data Text deriving Typeable

data instance Element Text elems e = Text T.Text

text :: T.Text -> Options '[Text] e
text = makeOption . Text

data FontSize deriving (Typeable)

data instance Element FontSize elems e = FontPixel Int | FontPercent Int

smallerFont :: Options '[FontSize] e
smallerFont = makeOption $ FontPercent 70

greaterFont :: Options '[FontSize] e
greaterFont = makeOption $ FontPercent 130

fontSizePx :: Int -> Options '[FontSize] e
fontSizePx = makeOption . FontPixel

data Orientation deriving (Typeable)

data instance Element Orientation elems e = Horizontal | Vertical

horizontal :: Options '[Orientation] e
horizontal = makeOption Horizontal

vertical :: Options '[Orientation] e
vertical = makeOption Vertical


data FontStyle deriving (Typeable)

data instance Element FontStyle elems e = ItalicStyle | RegularStyle

italicStyle :: Options '[FontStyle] e
italicStyle = makeOption ItalicStyle

regularStyle :: Options '[FontStyle] e
regularStyle = makeOption RegularStyle

data FontWeight deriving (Typeable)

data instance Element FontWeight elems e = FontWeight Int deriving (Typeable)

bold :: Options '[FontWeight] e
bold = makeOption $ FontWeight 700

regularWeight :: Options '[FontWeight] e
regularWeight = makeOption $ FontWeight 400

data FontColour deriving (Typeable)

data instance Element FontColour elems e = FontColour (Colour Double) deriving (Typeable)

fontColour :: Colour Double -> Options '[FontColour] e
fontColour = makeOption . FontColour

-- Events

data TextChange deriving (Typeable)

data instance Element TextChange elems e = TextChange (T.Text -> e)

onTextChange :: (T.Text -> e) -> Options '[TextChange] e
onTextChange = makeOption . TextChange

data Activate deriving (Typeable)

data instance Element Activate elems e = Activate e

onActivate :: e -> Options '[Activate] e
onActivate = makeOption . Activate

data Click deriving Typeable

data instance Element Click elems e = Click e

onClick :: e -> Options '[Click] e
onClick = makeOption . Click