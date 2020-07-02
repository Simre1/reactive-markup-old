{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Markup 
  ( 
  -- ** Markup
    Element
  , Markup
  , SimpleMarkup
  , toMarkup
  , toSimpleMarkup
  , expandMarkup
  -- ** Runner
  , Runner
  , emptyRunner
  , (|->)
  , reduceRunner
  , runMarkup
  , runMarkupWithTwo
  , runMarkupExact
  , runMarkupWithTwoExact
  -- ** MarkupBuilder
  , MarkupBuilder
  , emptyMarkupBuilder
  , (+->)
  , getMarkups
  , getSimpleMarkups
  -- ** Type families
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

-- | Data family for all elements. By adding instances, you can create more elements easily.
--  
-- elementType: Declares which type of 'Element' this is.
-- 
-- children: Declares which type of other 'Element's this 'Element' may hold.
-- 
-- event: The event that might be emitted from this event.
data family Element  (elementType :: *) (children :: [*]) event 

-- | 'Markup' wraps an 'Element'. elems corresponds to the elements that are directly wrapped.
-- 
-- elems: Correspnds to the 'Element's that are directly wrapped.
-- 
-- children: Sum of all children that any of the wrapped 'Element's can contain.
-- 
-- event: The event that might be emitted from this 'Markup'.
data Markup  (elems :: [*])  (children :: [*])  (event :: *) where 
  Markup :: (Typeable x, Functor (Element x children)) => Element x children event -> Markup elems children event

-- | A simplified version of 'Markup' where elems and children are the same.
type SimpleMarkup elems = Markup elems elems

instance Functor (Markup elems children) where
  fmap f (Markup elem) = Markup (fmap f elem)


-- | Wraps any 'Element' in 'Markup'.
toMarkup :: (Typeable t, Functor (Element t children)) => Element t children e -> Markup '[t] children e
toMarkup element = unsafeCoerce $ (Markup element)

-- | Transforms a 'Markup' to 'SimpleMarkup'.
toSimpleMarkup :: Markup elems children e -> SimpleMarkup (Merge elems children) e
toSimpleMarkup = unsafeCoerce

-- | You can always expand a 'Markup' so that it needs more capabilities as long as the original ones are covered.
-- Should be used carefully because it breaks type inference.
expandMarkup :: (SubList subElements fullElements, SubList subChildren fullChildren) => Markup subElements subChildren e -> Markup fullElements fullChildren e
expandMarkup = unsafeCoerce

-- | 'Markup' does not have a fixed implementation. This implementation is provided with a 'Runner' so that
-- the same 'Markup' can have multiple implementations. 
-- For example, you might want to use different 'Runners' depending on the platform your app is running on.
-- 
-- elems: Declares which 'Element's can be used with this 'Runner'
-- 
-- result: The result from running a 'Markup' with this 'Runner'.
data Runner (elems :: [*]) result where
  Runner :: HM.HashMap TypeRep (Function elems result) -> Runner elems result

data Function (elems :: [*]) result where
  Function :: (forall e. Element t elems e -> Runner (Merge '[t] elems) result -> (e -> IO ()) -> result) -> Function (Merge elems '[t]) result

-- | An empty Runner that cannot run anything.
emptyRunner :: Runner '[] result
emptyRunner = Runner HM.empty

-- | Allows a 'Runner' to handle one more element.
(|->) :: forall result t elems. (Typeable t) => 
  Runner elems result
   -> (forall event. Element t elems event -> Runner (Merge '[t] elems) result -> (event -> IO ()) -> result) -- ^ This function is used to handle 'Element's.
   -> Runner (Merge elems '[t]) result -- ^ Runner has additional capabilities and can now run 'Element' t.
(|->) (Runner hashMap) f = Runner (HM.insert (typeRep $ Proxy @t) (Function f) (unsafeCoerce hashMap))

-- | You can always reduce the capabilities of a 'Runner' as long as the resulting capabilities are within the original. 
-- Should be used carefully since it breaks type inference.
reduceRunner :: SubList elems2 elems1 => Runner elems1 result -> Runner elems2 result
reduceRunner runner = unsafeCoerce runner

-- | Runs a 'Markup' with the given 'Runner'. The types of 'Markup' and 'Runner' must match exactly.
runMarkupExact :: forall elems children e result. 
  Runner (Merge elems children) result 
  -> (e -> IO ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e -> result
runMarkupExact runner markup = runMarkupWithTwoExact (unsafeCoerce runner) (unsafeCoerce runner) markup

-- | Run the directly wrapped 'Element's of a 'Markup' with the first 'Runner' and their children with the second. The types of 'Markup' and 'Runner' must match exactly.
runMarkupWithTwoExact :: forall elems children e result. 
  Runner elems result -> Runner children result 
  -> (e -> IO ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e 
  -> result
runMarkupWithTwoExact (Runner hashMap) runner2 handleEvent (Markup elem) = runMarkupExact' elem
  where 
    runMarkupExact' :: forall x. Typeable x => Element x children e -> result
    runMarkupExact' elem = unwrap (hashMap HM.! (typeRep (Proxy @x)))
      where 
        unwrap :: Function exec result -> result
        unwrap (Function f) = unsafeCoerce f elem runner2 handleEvent

-- | The preferred way to run a 'Markup' with a 'Runner'.
-- Is is a less restrictive version of 'runMarkupExact', so that any 'Runner' which has sufficient capabilities can be used to run the given 'Markup'.
-- There may be problems regarding type inference, but it ought to work fine.
runMarkup :: forall exec elems children e result. SubList (Merge elems children) exec => 
  Runner exec result 
  -> (e -> IO ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e
  -> result
runMarkup runner = runMarkupExact (reduceRunner runner)

-- The preferred way to run a 'Markup' with two 'Runner's. The first 'Runner' is used for the directly wrapped
-- 'Element's and the second for their children.
-- Is is a less restrictive version of 'runMarkupWithTwoExact', so that any 'Runner's which have sufficient capabilities can be used to run the given 'Markup'.
-- There may be problems regarding type inference, but it ought to work fine.
runMarkupWithTwo :: forall exec1 exec2 elems children e result. (SubList elems exec1, SubList children exec2) => 
  Runner exec1 result -> Runner exec2 result 
  -> (e -> IO ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e -> result
runMarkupWithTwo runner1 runner2 = runMarkupWithTwoExact (reduceRunner runner1) (reduceRunner runner2)

-- | Used to Combine multiple 'Markup's.
data MarkupBuilder (elems :: [*]) (children :: [*]) e = MarkupBuilder [Markup elems children e]

instance Functor (MarkupBuilder elems elems2) where
  fmap f (MarkupBuilder markup) = MarkupBuilder $ fmap (fmap f) markup

-- | Empty markup builder with no elements.
emptyMarkupBuilder :: MarkupBuilder '[] '[] e
emptyMarkupBuilder = MarkupBuilder []

-- | Adds a 'Markup' to a 'MarkupBuilder' while properly updating its types. 
(+->) :: MarkupBuilder elems children e -> Markup elems2 children2 e -> MarkupBuilder (Merge elems elems2) (Merge children children2) e
(+->) (MarkupBuilder markups) markup = unsafeCoerce $ MarkupBuilder $ markups ++ [unsafeCoerce markup]

-- | Get the 'Markup's of a MarkupBuilder in a list, so that each 'Markup' has the same type.
getMarkups :: MarkupBuilder elems children e -> [Markup elems children e]
getMarkups (MarkupBuilder markups) = markups

-- | Get 'SimpleMarkup's instead of 'Markup's.
getSimpleMarkups :: MarkupBuilder elems children e -> [SimpleMarkup (Merge elems children) e]
getSimpleMarkups = fmap unsafeCoerce . getMarkups


-- Custom errors might make debugging harder for now. 

type family HasElement a (as :: [*]) (full :: [*]) :: Constraint where
  HasElement a (a:_) _ = True ~ True
  HasElement a (x:as) full = HasElement a as full
  -- HasElement a as full = GHC.TypeError (GHC.Text "Element " GHC.:<>: GHC.ShowType a GHC.:<>: GHC.Text " is not within " GHC.:<>: GHC.ShowType full GHC.:<>: GHC.Text "!")

-- | Calculates if a type-level list is a sub list of another one.
type family SubList (sub :: [*]) (full :: [*]) :: Constraint where
  SubList full full = True ~ True
  SubList (s:sub) full = (HasElement s full full, SubList sub full)
  SubList '[] _ = True ~ True
  -- SubList sub full = GHC.TypeError (GHC.ShowType sub GHC.:<>: GHC.Text " is not a sub list of " GHC.:<>: GHC.ShowType full GHC.:<>: GHC.Text "!")

-- | Merges two type-level sub-lists so that no element occurs twice and the given order is maintained.
type family Merge (xs :: [*]) (ys :: [*]) where
  Merge (x:xs) ys = x : Merge xs (Remove x ys)
  Merge '[] ys = ys

type family Remove x (xs :: [*]) where
  Remove x (x:as) = Remove x as
  Remove x (a:as) = a : Remove x as
  Remove x '[] = '[]
