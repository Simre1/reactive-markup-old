{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module ReactiveMarkup.Markup
  ( -- ** Markup
    Element,
    Markup,
    SimpleMarkup,
    toMarkup,
    toSimpleMarkup,
    expandMarkup,
    -- ** Runner
    Runner,
    emptyRunner,
    (|->),
    fullRun,
    simpleRun,
    eventRun,
    childrenRun,
    reduceRunner,
    mapRunnerResult,
    mapRunnerEvent,
    runMarkup,
    runMarkupWithTwo,
    runMarkupExact,
    runMarkupWithTwoExact,
    -- ** Make lists of markups
    (+:),
    (+++),
    noElems,
    -- ** Options
    Options (..),
    makeOption,
    (//),
    noOps,
    expandOptions,

    -- ** Type families
    SubList,
    type (|->),
    type (<+),
    Void,
    Typeable,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeRep)
import Data.Void (Void)
import GHC.Exts (Constraint)
import qualified GHC.TypeLits as GHC
import Unsafe.Coerce (unsafeCoerce)

-- | Data family for all elements. By adding instances, you can create more elements easily.
--
-- elementType: Declares which type of 'Element' this is.
--
-- children: Declares which type of other 'Element's this 'Element' may hold.
--
-- event: The event that might be emitted from this event.
data family Element (elementType :: *) (children :: [*]) event

-- | 'Markup' wraps an 'Element'. elems corresponds to the elements that are directly wrapped.
--
-- elems: Corresponds to the 'Element's that are directly wrapped.
--
-- children: Sum of all children that any of the wrapped 'Element's can contain.
--
-- event: The event that might be emitted from this 'Markup'.
data Markup (elems :: [*]) (children :: [*]) (event :: *) where
  Markup :: Typeable (GetId x) => Element x children internalEvent -> (internalEvent -> event) -> Markup elems children event

-- | A simplified version of 'Markup' where elems and children are the same.
type SimpleMarkup elems = Markup elems elems

instance Functor (Markup elems children) where
  fmap f (Markup elem mapEvent) = Markup elem (f . mapEvent)

-- | Wraps any 'Element' in 'Markup'.
toMarkup :: (Typeable (GetId t)) => Element t children e -> Markup '[t] children e
toMarkup element = Markup element id

-- | Transforms a 'Markup' to 'SimpleMarkup'.
toSimpleMarkup :: Markup elems children e -> SimpleMarkup (elems <+ children) e
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
  Function :: (Element t children e -> Runner children m result -> (e -> m) -> result) -> Function m result

-- | An empty Runner that cannot run anything.
emptyRunner :: Runner '[] m result
emptyRunner = Runner HM.empty

-- | Merge two 'Runner's.
(|->) ::
  forall m result elems1 elems2.
  Runner elems1 m result ->
  -- | This function is used to handle 'Element's.
  Runner elems2 m result ->
  -- | Runner has additional capabilities and can now run 'Element' t.
  Runner (elems1 |-> elems2) m result
(|->) (Runner hashMap1) (Runner hashMap2) = Runner $ HM.union hashMap2 hashMap1

-- | Create a new runner to handle the specified element with all parameters available.
fullRun ::
  forall t m result.
  Typeable (GetId t) =>
  (forall event children. Element t children event -> Runner children m result -> (event -> m) -> result) ->
  Runner '[t] m result
fullRun f = Runner (HM.insert (typeRep $ Proxy @(GetId t)) (Function $ unsafeCoerce f) HM.empty)

-- | Simplified version of 'fullRun', where only the element is available.
simpleRun ::
  Typeable (GetId t) =>
  (forall children event. Element t children event -> result) ->
  Runner '[t] m result
simpleRun f = fullRun (\e _ _ -> f e)

-- | Simplified version of 'fullRun', where only the element and the event handler is available.
eventRun ::
  Typeable (GetId t) =>
  (forall children event. Element t children event -> (event -> m) -> result) ->
  Runner '[t] m result
eventRun f = fullRun (\e _ h -> f e h)

-- | Simplified version of 'fullRun', where only the element and the children runner is available.
childrenRun ::
  Typeable (GetId t) =>
  (forall event children. Element t children event -> Runner children m result -> result) ->
  Runner '[t] m result
childrenRun f = fullRun (\e r _ -> f e r)

-- | You can always reduce the capabilities of a 'Runner' as long as the resulting capabilities are within the original.
-- Should be used carefully since it breaks type inference.
reduceRunner :: SubList elems2 elems1 => Runner elems1 m result -> Runner elems2 m result
reduceRunner runner = unsafeCoerce runner

-- | Given a bidirectional mapping, 'mapRunnerResult' allows to change the result of a ''Runner. 
mapRunnerResult :: forall result result2 elems m. (result -> result2) -> (result2 -> result) -> Runner elems m result -> Runner elems m result2
mapRunnerResult f1 f2 (Runner hashMap) = Runner $ HM.map mapFunction hashMap
  where 
    mapFunction :: Function m result -> Function m result2
    mapFunction (Function f) = Function $ \elem childRunner handle -> f1 $ f elem (editRunner childRunner) handle
      where 
        editRunner :: Runner children m result2 -> Runner children m result
        editRunner runner = mapRunnerResult f2 f1 runner

-- | Given a bidirectional mapping, 'mapRunnerEvent' allows to change the event handling function of a ''Runner. 
mapRunnerEvent :: forall result m1 m2 elems. (m1 -> m2) -> (m2 -> m1) -> Runner elems m1 result -> Runner elems m2 result
mapRunnerEvent f1 f2 (Runner hashMap) = Runner $ HM.map mapFunction hashMap
  where 
    mapFunction :: Function m1 result -> Function m2 result
    mapFunction (Function f) = Function $ \elem childRunner handle -> f elem (mapRunnerEvent f2 f1 childRunner) (f2 . handle)

-- | Runs a 'Markup' with the given 'Runner'. The types of 'Markup' and 'Runner' must match exactly.
runMarkupExact ::
  forall elems children e m result.
  Runner (elems |-> children) m result ->
  -- | Used to handle the event that this 'Markup' might emit.
  (e -> m) ->
  Markup elems children e ->
  result
runMarkupExact runner markup = runMarkupWithTwoExact (unsafeCoerce runner) (unsafeCoerce runner) markup

-- | Run the directly wrapped 'Element's of a 'Markup' with the first 'Runner' and their children with the second. The types of 'Markup' and 'Runner' must match exactly.
runMarkupWithTwoExact ::
  forall elems children e m result.
  Runner elems m result ->
  Runner children m result ->
  -- | Used to handle the event that this 'Markup' might emit.
  (e -> m) ->
  Markup elems children e ->
  result
runMarkupWithTwoExact (Runner hashMap) runner2 handleEvent (Markup elem mapEvent) = runMarkupExact' (elem) mapEvent
  where
    runMarkupExact' :: forall x internalEvent. Typeable (GetId x) => Element x children internalEvent -> (internalEvent -> e) -> result
    runMarkupExact' elem mapEvent = unwrap (hashMap HM.! (typeRep (Proxy @(GetId x))))
      where
        unwrap :: Function m result -> result
        unwrap (Function f) = unsafeCoerce f elem runner2 (handleEvent . mapEvent)

-- | The preferred way to run a 'Markup' with a 'Runner'.
-- Is is a less restrictive version of 'runMarkupExact', so that any 'Runner' which has sufficient capabilities can be used to run the given 'Markup'.
-- There may be problems regarding type inference, but it ought to work fine.
runMarkup ::
  forall exec elems children e m result.
  SubList (elems <+ children) exec =>
  Runner exec m result ->
  -- | Used to handle the event that this 'Markup' might emit.
  (e -> m) ->
  Markup elems children e ->
  result
runMarkup runner = runMarkupExact (unsafeCoerce runner)

-- | The preferred way to run a 'Markup' with two 'Runner's. The first 'Runner' is used for the directly wrapped
-- 'Element's and the second for their children.
-- Is is a less restrictive version of 'runMarkupWithTwoExact', so that any 'Runner's which have sufficient capabilities can be used to run the given 'Markup'.
-- There may be problems regarding type inference, but it ought to work fine.
runMarkupWithTwo ::
  forall exec1 exec2 elems children e m result.
  (SubList elems exec1, SubList children exec2) =>
  Runner exec1 m result ->
  Runner exec2 m result ->
  -- | Used to handle the event that this 'Markup' might emit.
  (e -> m) ->
  Markup elems children e ->
  result
runMarkupWithTwo runner1 runner2 = runMarkupWithTwoExact (reduceRunner runner1) (reduceRunner runner2)

-- | Prepend a `Markup` element to a list of `Markup`s.
(+:) :: Markup elems children e -> [Markup elems2 children2 e] -> [Markup (elems <+ elems2) (children <+ children2) e]
(+:) markup markups = unsafeCoerce markup : unsafeCoerce markups

-- | Concatenate two lists of `Markup`s.
(+++) :: [Markup elems children e] -> [Markup elems2 children2 e] -> [Markup (elems <+ elems2) (children <+ children2) e]
(+++) markups1 markups2 = unsafeCoerce markups1 ++ unsafeCoerce markups2

-- | Empty list of `Markup`s, which is sometimes needed for type inference.
noElems :: [Markup '[] '[] e]
noElems = []

infixr 2 +:
infixr 2 +++

-- | Wrapper for elements which should be used as options for other elements.
newtype Options (os :: [*]) e = Options [SimpleMarkup os e]

-- | Concatenate two `Options`.
(//) ::
  Options os1 e ->
  Options os2 e ->
  Options (os1 <+ os2) e
(//) (Options options1) (Options options2) = Options (options1 +++ options2)

-- | Empty `Options`.
noOps :: Options '[] e
noOps = Options []

-- | Wrap an element in `Options`.
makeOption :: Typeable (GetId t) => Element t '[] e -> Options '[t] e
makeOption elem = Options [toSimpleMarkup $ toMarkup elem]

-- | Expand the type of an `Options` similar to `expandMarkup`.
expandOptions :: SubList elems expanded => Options elems e -> Options expanded e
expandOptions = unsafeCoerce

-- TODO: Better Type errors!!!

-- | Calculates if a type-level list is a sub list of another one.
type SubList (sub :: [*]) (full :: [*]) = full ~ (full <+ sub)

-- | Used for merging the capabilities of 'Runner's. It is different to 'Merge' such that elements
-- where only a type-level argument differs do not get merged. Instead, only the last of such elements will
-- persist in the result.
type family RunnerMerge (xs :: [*]) (ys :: [*]) where
  RunnerMerge xs xs = xs
  RunnerMerge '[] ys = ys
  RunnerMerge xs '[] = xs
  RunnerMerge (x : xs) ys = MaybeAdd (Equal (Collect x ys) x) x (RunnerMerge xs (Remove x ys))

type as |-> bs = RunnerMerge as bs

type family Equal a b where
  Equal a a = 'True
  Equal _ _ = 'False

type family MaybeAdd (b :: Bool) x xs where
  MaybeAdd 'True x xs = x : xs
  MaybeAdd 'False _ xs = xs

-- | Used for merging the type-level arguments of 'Markup'. Some important properties are:
-- * It is assumed that the two given type-lists are valid.
-- * No element will occur twice after merging.
-- * The relative order of the elements is maintained.
-- * If there exist two elements with a type-level argument as their last argument, then those two elements
--   are replaced by a single one with a merged type-level list.

type family (<+) (as :: [*]) (bs :: [*]) where
  xs <+ xs = xs
  xs <+ '[] = xs
  '[] <+ ys = ys
  (x : xs) <+ ys = Collect x ys : (xs <+ (CollectRemove x ys))

type family MergeElements a b :: * where
  MergeElements a a = a
  MergeElements (f a) (f b) = f (a <+ b)
  MergeElements x _ = x

type family Collect a (as :: [*]) :: * where
  Collect a '[] = a
  Collect a (x : xs) = Collect (MergeElements a x) xs

type family CollectRemove a (as :: [*]) where
  CollectRemove _ '[] = '[]
  CollectRemove a (a : xs) = CollectRemove a xs
  CollectRemove (f (a :: [*])) (f _ : xs) = CollectRemove (f a) xs
  CollectRemove a (x : xs) = x : CollectRemove a xs

type family Remove x (xs :: [*]) where
  Remove x (x : as) = Remove x as
  Remove x (a : as) = a : Remove x as
  Remove x '[] = '[]

type family GetId x :: * where
  GetId (f a b c d (x :: [*])) = f a b c d '[]
  GetId (f a b c (x :: [*])) = f a b c '[]
  GetId (f a b (x :: [*])) = f a b '[]
  GetId (f a (x :: [*])) = f a '[]
  GetId (f (x :: [*])) = f '[]
  GetId x = x
