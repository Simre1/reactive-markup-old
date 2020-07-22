{-# LANGUAGE UndecidableInstances #-}

module ReactiveMarkup.Elements.Settings where
import Data.Colour.SRGB (Colour)

import qualified Data.Text as T
import ReactiveMarkup.Markup
import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)

data SpecificOptions t (options :: [*])

data instance Element (SpecificOptions t options) elems e = SpecificOptions [Markup options '[] e] (Markup '[t] elems e)

data GeneralOptions (options :: [*])

data instance Element (GeneralOptions options) elems e
  = GeneralOptions [Markup options '[] e] (SimpleMarkup elems e)

data Options t specific general e = Options [Markup specific '[] e] [Markup general '[] e]

data Any deriving Typeable

-- applyOptionCollector ::
--   CollectOptions specific general elems children =>
--   Options t specific general e ->
--   Markup elems children e ->
--   CollectedOptions t specific general elems children e
-- applyOptionCollector = collect

class CollectOptions (specific :: [*]) (general :: [*]) (elems :: [*]) (children :: [*]) where
  type CollectedOptions specific general elems children :: * -> *
  type CollectElement specific elems :: *
  collect :: Options (CollectElement specific elems) specific general e -> Markup elems children e -> CollectedOptions specific general elems children e

instance Typeable (g : general) => CollectOptions '[] (g : general) (elems) children where
  type CollectedOptions _ (g : general) (elems) children = Markup '[GeneralOptions (g : general)] (Merge (elems) children)
  type CollectElement '[] _ = Any
  collect (Options _ general) = toMarkup . GeneralOptions general . toSimpleMarkup

instance (Typeable t, Typeable (s : specific)) => CollectOptions (s : specific) '[] '[t] children where
  type CollectedOptions (s : specific) _ '[t] (children) = Markup '[SpecificOptions t (s : specific)] children
  type CollectElement (s:specific) '[t] = t
  collect (Options specific _) = toMarkup . SpecificOptions specific

instance CollectOptions '[] '[] elems children where
  type CollectedOptions _ _ elems children = Markup elems children
  type CollectElement '[] _ = Any 
  collect _ markup = markup

instance (Typeable (g : general), Typeable (s : specific), Typeable t) => CollectOptions (s : specific) (g : general) '[t] children where
  type
    CollectedOptions (s : specific) (g : general) '[t] children =
      Markup '[GeneralOptions (g : general)] (Merge '[SpecificOptions t (s : specific)] children)
  type CollectElement (s:specific) '[t] = t
  collect (Options specific general) =
    toMarkup . GeneralOptions general . toSimpleMarkup . toMarkup . SpecificOptions specific

(%%) :: forall t specific1 general1 e specific2 general2. Options t specific1 general1 e -> Options t specific2 general2 e -> Options t (Merge specific1 specific2) (Merge general1 general2) e
(%%) (Options specific1 general1) (Options specific2 general2) = Options (unsafeCoerce specific1 ++ unsafeCoerce specific2) (unsafeCoerce general1 ++ unsafeCoerce general2)

infix 4 %%

(%->) ::
  CollectOptions specific general elems children =>
  Options (CollectElement specific elems) specific general e ->
  Markup elems children e ->
  CollectedOptions specific general elems children e
(%->) = collect

infix 3 %->

makeGeneralOption :: Markup '[o] '[] e ->  Options t '[] '[o] e 
makeGeneralOption markup = Options [] [markup]

makeSpecificOption :: Markup '[o] '[] e -> Options t '[o] '[] e
makeSpecificOption markup = Options [markup] [] 

data FontSize = FontSize' (Int -> Int) deriving (Typeable)

data instance Element FontSize elems e = FontSize FontSize deriving (Typeable)

data Orientation = Horizontal | Vertical deriving Typeable
data instance Element Orientation elems e = Orientation Orientation

data FontStyle = ItalicStyle | RegularStyle deriving Typeable 
data instance Element FontStyle elems e = FontStyle FontStyle deriving Typeable

data FontWeight = FontWeight' Int deriving Typeable
data instance Element FontWeight elems e = FontWeight FontWeight deriving Typeable

data FontColour = FontColour' (Colour Double) deriving Typeable
data instance Element FontColour elems e = FontColour FontColour deriving Typeable


-- Styles 
smallerFont :: Options t '[] '[FontSize] e
smallerFont = makeGeneralOption . toMarkup $ FontSize $ FontSize' (\x -> round $ fromIntegral x * 0.7)

greaterFont :: Options t '[] '[FontSize] e
greaterFont = makeGeneralOption . toMarkup $ FontSize $ FontSize' (\x -> round $ fromIntegral x * 1.3)

fontSizePx :: Int -> Options t '[] '[FontSize] e
fontSizePx px = makeGeneralOption . toMarkup $ FontSize $ FontSize' (const px)

horizontal :: Options t '[] '[Orientation] e
horizontal = makeGeneralOption . toMarkup $ Orientation Horizontal

vertical :: Options t '[] '[Orientation] e
vertical = makeGeneralOption . toMarkup $ Orientation Vertical

italicStyle :: Options t '[] '[FontStyle] e
italicStyle = makeGeneralOption . toMarkup $ FontStyle ItalicStyle

regularStyle :: Options t '[] '[FontStyle] e
regularStyle = makeGeneralOption . toMarkup $ FontStyle RegularStyle

bold :: Options t '[] '[FontWeight] e
bold = makeGeneralOption . toMarkup $ FontWeight $ FontWeight' 700

regularWeight :: Options t '[] '[FontWeight] e
regularWeight = makeGeneralOption . toMarkup $ FontWeight $ FontWeight' 400

fontColour :: Colour Double -> Options t '[] '[FontColour] e
fontColour = makeGeneralOption . toMarkup . FontColour . FontColour'

-- Events

data TextChange deriving (Typeable)

data instance Element TextChange elems e = TextChange (T.Text -> e)

data Activate deriving (Typeable)

data instance Element Activate elems e = Activate e

onTextChange :: (T.Text -> e) -> Options t '[TextChange] '[] e
onTextChange = makeSpecificOption . toMarkup . TextChange

onActivate :: e -> Options t '[Activate] '[] e
onActivate = makeSpecificOption . toMarkup . Activate