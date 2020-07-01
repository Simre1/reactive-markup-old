{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Markup 
  ( Markup
  , SimpleMarkup
  , Element
  , toMarkup
  , Runner
  , emptyRunner
  , (|->)
  , reduceRunner
  , runMarkup
  , runMarkupWithTwo
  , runMarkupExact
  , runMarkupWithTwoExact
  , MarkupBuilder
  , emptyMarkupBuilder
  , (+->)
  , getMarkups
  , getSimpleMarkups
  , expandMarkup
  , toSimpleMarkup
  , Combine
  , SubList
  , Merge
  , Void
  , Typeable
  )
  where

import Unsafe.Coerce (unsafeCoerce)
import qualified Data.HashMap.Strict as HM
import Data.Typeable (typeRep, Typeable, TypeRep, Proxy(..))
import qualified GHC.TypeLits as GHC
import GHC.Exts (Constraint)
import Data.Void (Void)

data Markup (elems :: [*]) (children :: [*]) (event :: *) where
  Markup :: (Typeable x, Functor (Element x children)) => Element x children event -> Markup elems children event

type SimpleMarkup elems = Markup elems elems

instance Functor (Markup elemens children) where
  fmap f (Markup elem) = Markup (fmap f elem)


data family Element (t :: *) (elems :: [*]) e


data Runner (ts :: [*]) b where
  Runner :: HM.HashMap TypeRep (Function ts b) -> Runner ts b

data Function (elems :: [*]) b where
  Function :: (forall e. Element t elems e -> Runner (Merge '[t] elems) b -> (e -> IO ()) -> b) -> Function (Merge elems '[t]) b


data MarkupBuilder (elems :: [*]) (children :: [*]) e = MarkupBuilder [Markup elems children e]

instance Functor (MarkupBuilder elems elems2) where
  fmap f (MarkupBuilder markup) = MarkupBuilder $ fmap (fmap f) markup


toMarkup :: (Typeable t, Functor (Element t children)) => Element t children e -> Markup '[t] children e
toMarkup element = unsafeCoerce $ (Markup element)

toSimpleMarkup :: Markup elems children e -> SimpleMarkup (Merge elems children) e
toSimpleMarkup = unsafeCoerce

emptyRunner :: Runner '[] b
emptyRunner = Runner HM.empty

(|->) :: forall b t elems. (Typeable t) => 
  Runner elems b -> (forall event. Element t elems event -> Runner (Merge '[t] elems) b -> (event -> IO ()) -> b) -> Runner (Merge elems '[t]) b
(|->) (Runner hashMap) f = Runner (HM.insert (typeRep $ Proxy @t) (Function f) (unsafeCoerce hashMap))

reduceRunner :: SubList elems2 elems1 => Runner elems1 b -> Runner elems2 b
reduceRunner runner = unsafeCoerce runner

runMarkupExact :: forall elems children e b. Runner (Merge elems children) b -> (e -> IO ()) -> Markup elems children e -> b
runMarkupExact runner markup = runMarkupWithTwoExact (unsafeCoerce runner) (unsafeCoerce runner) markup

runMarkupWithTwoExact :: forall elems children e b. Runner elems b -> Runner children b -> (e -> IO ()) -> Markup elems children e -> b
runMarkupWithTwoExact (Runner hashMap) runner2 handleEvent (Markup elem) = runMarkupExact' elem
  where 
    runMarkupExact' :: forall x. Typeable x => Element x children e -> b
    runMarkupExact' elem = unwrap (hashMap HM.! (typeRep (Proxy @x)))
      where 
        unwrap :: Function exec b -> b
        unwrap (Function f) = unsafeCoerce f elem runner2 handleEvent

runMarkup :: forall exec elems children e b. SubList (Merge elems children) exec => Runner exec b -> (e -> IO ()) -> Markup elems children e -> b
runMarkup runner = runMarkupExact (reduceRunner runner)

runMarkupWithTwo :: forall exec1 exec2 elems children e b. (SubList elems exec1, SubList children exec2) => Runner exec1 b -> Runner exec2 b -> (e -> IO ()) -> Markup elems children e -> b
runMarkupWithTwo runner1 runner2 = runMarkupWithTwoExact (reduceRunner runner1) (reduceRunner runner2)

emptyMarkupBuilder :: MarkupBuilder '[] '[] e
emptyMarkupBuilder = MarkupBuilder []

(+->) :: MarkupBuilder elems children e -> Markup elems2 children2 e -> MarkupBuilder (Merge elems elems2) (Merge children children2) e
(+->) (MarkupBuilder markups) markup = unsafeCoerce $ MarkupBuilder $ markups ++ [unsafeCoerce markup]

getMarkups :: MarkupBuilder elems children e -> [Markup elems children e]
getMarkups (MarkupBuilder markups) = markups

getSimpleMarkups :: MarkupBuilder elems children e -> [SimpleMarkup (Merge elems children) e]
getSimpleMarkups = fmap unsafeCoerce . getMarkups

class (SubList elems1 combined, SubList elems2 combined) => Combine (elems1 :: [*]) (elems2 :: [*]) (combined :: [*]) | elems1 elems2 -> combined where

instance (SubList elems1 combined, SubList elems2 combined, combined ~ Merge elems1 elems2) => Combine elems1 elems2 combined where

type family InsertElement a as where
  InsertElement a (a:as) = (a:as)
  InsertElement a (x:as) = x : InsertElement a as
  InsertElement a '[] = '[a]

type family HasElement a (as :: [*]) :: Constraint where
  HasElement a (a:_) = True ~ True
  HasElement a (x:as) = HasElement a as
  HasElement a as = GHC.TypeError (GHC.Text "Element " GHC.:<>: GHC.ShowType a GHC.:<>: GHC.Text " is not supported by runner " GHC.:<>: GHC.ShowType as GHC.:<>: GHC.Text "!")

type family SubList (sub :: [*]) (full :: [*]) where
  SubList full full = True ~ True
  SubList (s:sub) full = (HasElement s full, SubList sub full)
  SubList '[] _ = True ~ True
  SubList sub full = GHC.TypeError (GHC.ShowType sub GHC.:<>: GHC.Text " is not supported by runner " GHC.:<>: GHC.ShowType full GHC.:<>: GHC.Text "!")


type family Merge (xs :: [*]) (ys :: [*]) where
  Merge (x:xs) ys = x : Merge xs (Remove x ys)
  Merge '[] ys = ys

type family Remove x (xs :: [*]) where
  Remove x (x:as) = Remove x as
  Remove x (a:as) = a : Remove x as
  Remove x '[] = '[]


-- | Should be used carefully because it breaks type inference.
expandMarkup :: (SubList subElements fullElements, SubList subChildren fullChildren) => Markup subElements subChildren e -> Markup fullElements fullChildren e
expandMarkup = unsafeCoerce
