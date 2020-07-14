module ReactiveMarkup.Elements.Settings 
  ( Setting
  , makeSetting
  , applySetting
  , Flag(..)
  , Orientation(..)
  , Italic(..)
  , Bold(..)
  , Size(..)
  )
  where

newtype Setting a = Setting (a -> a)

instance Semigroup (Setting a) where
  (Setting f1) <> (Setting f2) = Setting $ f2 . f1

instance Monoid (Setting a) where
  mempty = Setting id

makeSetting :: (a -> a) -> Setting a
makeSetting = Setting

applySetting :: a -> Setting a -> a
applySetting a (Setting f) = f a

data Flag setting = Enabled | Disabled deriving (Eq, Show)

data Orientation = Horizontal | Vertical deriving (Eq, Show)

data Italic = Italic | NotItalic

data Bold = Bold | NotBold

data Size = Smallest | Smaller | Regular | Bigger | Biggest