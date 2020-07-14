{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module ReactiveMarkup.Markup 
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
-- elems: Corresponds to the 'Element's that are directly wrapped.
-- 
-- children: Sum of all children that any of the wrapped 'Element's can contain.
-- 
-- event: The event that might be emitted from this 'Markup'.
data Markup (elems :: [*])  (children :: [*])  (event :: *) where 
  Markup :: Typeable x => Element x children internalEvent -> (internalEvent -> event) -> Markup elems children event

-- | A simplified version of 'Markup' where elems and children are the same.
type SimpleMarkup elems = Markup elems elems

instance Functor (Markup elems children) where
  fmap f (Markup elem mapEvent) = Markup elem (f . mapEvent)


-- | Wraps any 'Element' in 'Markup'.
toMarkup :: (Typeable t) => Element t children e -> Markup '[t] children e
toMarkup element = Markup element id

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
data Runner (elems :: [*]) m result where
  Runner :: HM.HashMap TypeRep (Function m result) -> Runner elems m result

data Function m result where
  Function :: (Element t children e -> Runner children m result -> (e -> m ()) -> m result) -> Function m result

data Children

-- | An empty Runner that cannot run anything.
emptyRunner :: Runner '[] m result
emptyRunner = Runner HM.empty

-- | Allows a 'Runner' to handle one more element.
(|->) :: forall t m result elems event. (Typeable t) => 
  Runner elems m result
   -> (Element t '[Children] event -> Runner '[Children] m result -> (event -> m ()) -> m result) -- ^ This function is used to handle 'Element's.
   -> Runner (Merge elems '[t]) m result -- ^ Runner has additional capabilities and can now run 'Element' t.
(|->) (Runner hashMap) f = Runner (HM.insert (typeRep $ Proxy @t) (Function $ unsafeCoerce f) (hashMap))

-- | You can always reduce the capabilities of a 'Runner' as long as the resulting capabilities are within the original. 
-- Should be used carefully since it breaks type inference.
reduceRunner :: SubList elems2 elems1 => Runner elems1 m result -> Runner elems2 m result
reduceRunner runner = unsafeCoerce runner

-- | Runs a 'Markup' with the given 'Runner'. The types of 'Markup' and 'Runner' must match exactly.
runMarkupExact :: forall elems children e m result. 
  Runner (Merge elems children) m result 
  -> (e -> m ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e -> m result
runMarkupExact runner markup = runMarkupWithTwoExact (unsafeCoerce runner) (unsafeCoerce runner) markup

-- | Run the directly wrapped 'Element's of a 'Markup' with the first 'Runner' and their children with the second. The types of 'Markup' and 'Runner' must match exactly.
runMarkupWithTwoExact :: forall elems children e m result. 
  Runner elems m result -> Runner children m result 
  -> (e -> m ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e 
  -> m result
runMarkupWithTwoExact (Runner hashMap) runner2 handleEvent (Markup elem mapEvent) = runMarkupExact' (elem) mapEvent
  where 
    runMarkupExact' :: forall x internalEvent. Typeable x => Element x children internalEvent -> (internalEvent -> e) -> m result
    runMarkupExact' elem mapEvent = unwrap (hashMap HM.! (typeRep (Proxy @x)))
      where 
        unwrap :: Function m result -> m result
        unwrap (Function f) = unsafeCoerce f elem runner2 (handleEvent . mapEvent)

-- | The preferred way to run a 'Markup' with a 'Runner'.
-- Is is a less restrictive version of 'runMarkupExact', so that any 'Runner' which has sufficient capabilities can be used to run the given 'Markup'.
-- There may be problems regarding type inference, but it ought to work fine.
runMarkup :: forall exec elems children e m result. SubList (Merge elems children) exec => 
  Runner exec m result 
  -> (e -> m ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e
  -> m result
runMarkup runner = runMarkupExact (reduceRunner runner)

-- The preferred way to run a 'Markup' with two 'Runner's. The first 'Runner' is used for the directly wrapped
-- 'Element's and the second for their children.
-- Is is a less restrictive version of 'runMarkupWithTwoExact', so that any 'Runner's which have sufficient capabilities can be used to run the given 'Markup'.
-- There may be problems regarding type inference, but it ought to work fine.
runMarkupWithTwo :: forall exec1 exec2 elems children e m result. (SubList elems exec1, SubList children exec2) => 
  Runner exec1 m result -> Runner exec2 m result 
  -> (e -> m ()) -- ^ Used to handle the event that this 'Markup' might emit.
  -> Markup elems children e -> m result
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


-- | Calculates if a type-level list is a sub list of another one.
type SubList (sub :: [*]) (full :: [*]) = full ~ Merge full sub

-- | Merges two type-level sub-lists so that no element occurs twice and the given order is maintained.
type family Merge (xs :: [*]) (ys :: [*]) where
  Merge xs xs = xs
  Merge (x:xs) ys = x : Merge xs (Remove x ys)
  Merge '[] ys = ys

type family Remove x (xs :: [*]) where
  Remove x (x:as) = Remove x as
  Remove x (a:as) = a : Remove x as
  Remove x '[] = '[]
  