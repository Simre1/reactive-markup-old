{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Currently in Review phase, since SimpleEvents might not be needed.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReactiveMarkup.SimpleEvents
  ( -- * EventSetup
    EventSetup,
    MonadES (..),

    -- * Events & Behaviors
    Event,
    EventHandler,
    EventTrigger,
    Behavior,
    Dynamic,

    -- * Combinators
    stepEvents,
    accumEvents,
    link,
    current,
    apply,
    toBehavior,
    toEvent,
    stateKeepingEvents,

    -- ** Filtering
    filterE,
    filterApply,
    ioFilterE,

    -- ** Switching

    -- | Allows easy management of nested 'Event's and 'Dynamic's.
    joinEvents,
    switchEvents,
    switchDynamics,

    -- * Making bindings
    newEvent,
    newDynamic,
    reactimate,
    modifyEvent,
    triggerEvent,
    runIOInES,
    customBehavior,
    customEvent,
    customDynamic,
    -- ** Creating Event Handlers
    makeEventHandler,
    simpleEventHandler,
    foldingEventHandler,
    filteringEventHandler,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Contravariant
import Data.IORef
import Data.IntMap as IM

-- | 'EventSetup' is used for creating the event network. Note that you can run 'EventSetup' in 'IO', but not the other way around. This may be used to set up events in a somewhat pure and predictable environment.
newtype EventSetup a = EventSetup {runEventSetup :: IO a}
  deriving (Functor, Applicative, Monad, Semigroup, Monoid, Alternative, MonadPlus)

-- | 'MonadES' works similarly to 'MonadIO' and allows you to execute 'EventSetup' in various contexts.
class MonadES m where
  liftES :: EventSetup a -> m a

instance MonadES EventSetup where
  liftES = id

instance MonadES IO where
  liftES = runEventSetup

-- | 'Event's occure at some points in time and carry some payload a. You must add 'EventHandler's to an 'Event' to get access to their payload.
newtype Event a = Event {registerHandler :: EventHandler a -> EventSetup (EventSetup ())}

-- | An 'EventHandler' describes how to deal with incoming events.
newtype EventHandler a = EventHandler {handleEvent :: a -> IO (EventHandler a)}

-- | An 'EventTrigger' triggers its corresponding event.
newtype EventTrigger a = EventTrigger {triggerEvent :: a -> IO ()}

-- | A 'Behavior' is a changing value that is always accessible. However, it is not possible to listen to state changes of a 'Behavior'.
newtype Behavior a = Behavior {behaviorValue :: EventSetup a}
  deriving (Functor, Applicative, Monad, Semigroup, Monoid)

-- | A 'Dynamic' is a combination of 'Event' and 'Behavior'.
data Dynamic a = Dynamic {toBehavior :: Behavior a, toEvent :: Event a}

current :: (MonadES m) => Behavior a -> m a
current = liftES . behaviorValue

apply :: Behavior (a -> b) -> Event a -> Event b
apply behavior (Event reg) = Event $ \handlerB -> reg $ makeHandler handlerB
  where
    makeHandler handlerB = EventHandler $ \a -> do
      f <- current behavior
      newHandlerB <- handleEvent handlerB (f a)
      pure $ makeHandler newHandlerB

filterE :: (a -> Bool) -> Event a -> Event a
filterE check (Event reg) = Event $ reg . filteringEventHandler (pure check)

filterApply :: Behavior (a -> Bool) -> Event a -> Event a
filterApply = ioFilterE . current

ioFilterE :: IO (a -> Bool) -> Event a -> Event a
ioFilterE check (Event reg) = Event $ reg . filteringEventHandler check

link :: MonadES m => Event a -> EventTrigger a -> m (EventSetup ())
link ev tr = reactimate ev (simpleEventHandler $ triggerEvent tr)

stepEvents :: MonadES m => Event a -> a -> m (Dynamic a)
stepEvents ev initial = liftES $ do
  bIORef <- runIOInES $ newIORef initial
  reactimate ev (simpleEventHandler $ writeIORef bIORef)
  pure $ Dynamic (Behavior . runIOInES $ readIORef bIORef) ev

accumEvents :: MonadES m => Event (a -> a) -> a -> m (Dynamic a)
accumEvents ev initial = liftES $ do
  bIORef <- runIOInES $ newIORef initial
  reactimate ev (simpleEventHandler $ \f -> (f $) <$> readIORef bIORef >>= writeIORef bIORef)
  pure $ Dynamic (Behavior . runIOInES $ readIORef bIORef) (apply (Behavior $ const <$> runIOInES (readIORef bIORef)) ev)

-- Switching

joinEvents :: Event (Event a) -> Event a
joinEvents evs = Event $ \triggerA -> do
  ref <- runIOInES $ newIORef mempty
  unReg <- reactimate evs (makeHandler ref triggerA)
  pure $ unReg *> runIOInES (join (readIORef ref))
  where
    makeHandler currentUnReg handlerA = simpleEventHandler $ \ev -> do
      join $ readIORef currentUnReg
      unReg <- liftES <$> reactimate ev handlerA
      writeIORef currentUnReg unReg

switchEvents :: Dynamic (Event a) -> Event a
switchEvents (Dynamic bE evs) = Event $ \handlerA -> do
  ref <- runIOInES $ newIORef mempty
  firstEvent <- current bE
  unregFirstEvent <- reactimate firstEvent handlerA
  runIOInES $ writeIORef ref $ liftES unregFirstEvent
  unReg <- reactimate evs $ makeHandler ref handlerA
  pure $ unReg *> runIOInES (join (readIORef ref))
  where
    makeHandler currentUnReg handlerA = simpleEventHandler $ \ev -> do
      join $ readIORef currentUnReg
      unReg <- liftES <$> reactimate ev handlerA
      writeIORef currentUnReg unReg

switchDynamics :: Dynamic (Dynamic a) -> Dynamic a
switchDynamics ds@(Dynamic bE evs) =
  let events = switchEvents $ toEvent <$> ds
      behavior = join $ toBehavior <$> bE
   in Dynamic behavior events

-- Make bindings

makeEventHandler :: (a -> IO (EventHandler a)) -> EventHandler a
makeEventHandler = EventHandler

simpleEventHandler :: (a -> IO ()) -> EventHandler a
simpleEventHandler f = EventHandler $ \a -> simpleEventHandler f <$ f a

foldingEventHandler :: (a -> a -> a) -> a -> EventHandler a -> EventHandler a
foldingEventHandler fold' acc handlerA = EventHandler $ \a -> do
  newHandlerF <- handleEvent handlerA (fold' a acc)
  pure $ foldingEventHandler fold' (fold' a acc) newHandlerF

filteringEventHandler :: IO (a -> Bool) -> EventHandler a -> EventHandler a
filteringEventHandler check handlerA = EventHandler $ \a -> do
  shouldTrigger <- check
  if shouldTrigger a
    then handleEvent handlerA a
    else pure handlerA

newEvent :: MonadES m => m (Event a, EventTrigger a)
newEvent = liftES . runIOInES $ do
  ref <- newIORef (IM.empty, 0)
  let onChange action = do
        eventHandlerInc <- runIOInES $ atomicModifyIORef' ref (\(fl, inc) -> ((insert inc action fl, succ inc), inc))
        pure $ runIOInES $ modifyIORef' ref $ first (delete eventHandlerInc)
      fire val = readIORef ref >>= traverse (\(EventHandler f) -> f val) . fst >>= (\newMap -> modifyIORef ref (first $ const newMap))
  pure (Event onChange, EventTrigger fire)

newDynamic :: MonadES m => a -> m (Dynamic a, EventTrigger a)
newDynamic i = liftES $ do
  (e, f) <- newEvent
  d <- stepEvents e i
  pure (d, f)

-- | Execute the 'EventHandler' whenever an 'Event' happens.
reactimate :: MonadES m => Event a -> EventHandler a -> m (EventSetup ())
reactimate (Event reg) = liftES . reg

-- | Allows you to run 'IO' in 'EventSetup'. Use with caution as arbitrary 'IO' in 'EventSetup' undermines its predictability and might cause problems for developers who expect that 'EventSetup' is only used for creating the event network.
runIOInES :: IO a -> EventSetup a
runIOInES = EventSetup

customEvent :: (EventHandler a -> IO (IO ())) -> Event a
customEvent f = Event $ fmap runIOInES . runIOInES . f

customBehavior :: IO a -> Behavior a
customBehavior = Behavior . runIOInES

customDynamic :: Behavior a -> Event a -> Dynamic a
customDynamic = Dynamic

-- | Allows events to know the value of their previous execution!
--
-- Transforms an event that just emits the function (a -> a) to an event that composes all (a -> a) together.
stateKeepingEvents :: Event (a -> a) -> Event (a -> a)
stateKeepingEvents (Event reg) = Event $ reg . foldingEventHandler (.) id

modifyEvent :: (EventHandler b -> EventHandler a) -> Event a -> Event b
modifyEvent f (Event reg) = Event $ reg . f

instance Contravariant EventHandler where
  contramap f (EventHandler handle) = EventHandler $ fmap (contramap f) . handle . f

instance Semigroup (EventHandler a) where
  (EventHandler f1) <> (EventHandler f2) = EventHandler $ \a -> f1 a <> f2 a

instance Monoid (EventHandler a) where
  mempty = EventHandler $ const mempty

instance Contravariant EventTrigger where
  contramap f (EventTrigger handle) = EventTrigger $ handle . f

instance Semigroup (EventTrigger a) where
  (EventTrigger f1) <> (EventTrigger f2) = EventTrigger $ \a -> f1 a <> f2 a

instance Monoid (EventTrigger a) where
  mempty = EventTrigger $ const mempty

instance Functor Event where
  fmap f (Event reg) = Event $ reg . contramap f

instance Semigroup (Event a) where
  (Event r1) <> (Event r2) = Event $ \handler -> do
    unR1 <- r1 handler
    unR2 <- r2 handler
    pure (unR1 *> unR2)

instance Monoid (Event a) where
  mempty = Event $ const (pure (pure ()))

instance Functor Dynamic where
  fmap f (Dynamic a e) = Dynamic (f <$> a) (f <$> e)

instance Semigroup a => Semigroup (Dynamic a) where
  d1 <> d2 = (<>) <$> d1 <*> d2

instance Monoid a => Monoid (Dynamic a) where
  mempty = Dynamic mempty mempty

instance Applicative Dynamic where
  pure a = Dynamic (pure a) mempty
  (Dynamic acc1 (Event reg1)) <*> (Dynamic acc2 (Event reg2)) = Dynamic (acc1 <*> acc2) applicativeEvent
    where
      applicativeEvent = Event $ \f -> do
        unReg1 <- reg1 $ makeHandler (\ab -> (\a -> ab a) <$> current acc2) f
        unReg2 <- reg2 $ makeHandler (\a -> (\ab -> ab a) <$> current acc1) f
        pure (unReg1 *> unReg2)
        where
          makeHandler getFull handlerFull = EventHandler $ \a -> do
            full <- getFull a
            nextHandlerFull <- handleEvent handlerFull full
            pure $ makeHandler getFull handlerFull
