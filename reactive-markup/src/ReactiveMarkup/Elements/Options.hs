{-# LANGUAGE UndecidableInstances #-}

module ReactiveMarkup.Elements.Options
where

import Data.Colour.SRGB (Colour)
import qualified Data.Text as T
import Data.Typeable
import ReactiveMarkup.Markup
import Unsafe.Coerce (unsafeCoerce)

data SizeUnit = Pixel Int | Percent Int

data Text deriving Typeable

data instance Element Text elems e = Text T.Text

text :: T.Text -> Options '[Text] e
text = makeOption . Text

data FontSize deriving (Typeable)

data instance Element FontSize elems e = FontSize SizeUnit

smallerFont :: Options '[FontSize] e
smallerFont = makeOption $ FontSize $ Percent 70

greaterFont :: Options '[FontSize] e
greaterFont = makeOption $ FontSize $ Percent 130

fontSizePx :: Int -> Options '[FontSize] e
fontSizePx = makeOption . FontSize . Pixel

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

data BackgroundColour deriving Typeable

data instance Element BackgroundColour elems e = BackgroundColour (Colour Double)

backgroundColour :: Colour Double -> Options '[BackgroundColour] e
backgroundColour = makeOption . BackgroundColour

data MinWidth deriving Typeable

data instance Element MinWidth elems e = MinWidth SizeUnit

minWidthPx :: Int -> Options '[MinWidth] e
minWidthPx = makeOption . MinWidth . Pixel

data MinHeight deriving Typeable

data instance Element MinHeight elems e = MinHeight SizeUnit

minHeightPx :: Int -> Options '[MinHeight] e
minHeightPx = makeOption . MinHeight . Pixel

data HomogenousRows deriving Typeable

data instance Element HomogenousRows elems e = HomogenousRows

homogenousRows :: Options '[HomogenousRows] e
homogenousRows = makeOption HomogenousRows

data HomogenousColumns deriving Typeable

data instance Element HomogenousColumns elems e = HomogenousColumns

homogenousColumns :: Options '[HomogenousColumns] e
homogenousColumns = makeOption HomogenousColumns

data HorizontalExpand deriving Typeable

data instance Element HorizontalExpand elems e = HorizontalExpand Bool

horizontalExpand :: Bool -> Options '[HorizontalExpand] e
horizontalExpand = makeOption . HorizontalExpand

data VerticalExpand deriving Typeable

data instance Element VerticalExpand elems e = VerticalExpand Bool

verticalExpand :: Bool -> Options '[VerticalExpand] e
verticalExpand = makeOption . VerticalExpand

expand :: Bool -> Options '[HorizontalExpand, VerticalExpand] e
expand b = horizontalExpand b %% verticalExpand b

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
