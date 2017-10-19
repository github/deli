{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Concurrent
    ( Channel
    , Time
    , Duration
    , ConcurrentT
    , addDuration
    , subtractTime
    , fork
    , sleep
    , yield
    , schedule
    , lazySchedule
    , now
    , newChannel
    , writeChannel
    , readChannel
    , runConcurrentT
    ) where

import Control.Lens (at, ix, makeLenses, to, use, (^?), (.=), (+=), (%=), (?~))
--import Control.Monad.Cont (MonadCont(..))
import Control.Monad.State.Strict
--import Control.Monad.Trans.Cont (resetT, shiftT)
import Data.Map.Strict
import Data.Maybe
import Data.PQueue.Min as PQueue
import Data.Sequence
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import qualified Control.Monad.Fail as Fail

------------------------------------------------------------------------------
-- ## Our Version of ContT
------------------------------------------------------------------------------

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

evalContT :: (Monad m) => ContT r m r -> m r
evalContT m = runContT m return
{-# INLINE evalContT #-}

shiftT :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (evalContT . f)
{-# INLINE shiftT #-}

resetT :: (Monad m) => ContT r m r -> ContT r' m r
resetT = lift . evalContT
{-# INLINE resetT #-}

instance Functor (ContT r m) where
    fmap f m = ContT $ \ c -> runContT m (c . f)
    {-# INLINE fmap #-}

instance Applicative (ContT r m) where
    pure x  = ContT ($ x)
    {-# INLINE pure #-}
    f <*> v = ContT $ \ c -> runContT f $ \ g -> runContT v (c . g)
    {-# INLINE (<*>) #-}
    --m *> k = m >>= \_ -> k
    m *> k = m >>= const k
    {-# INLINE (*>) #-}

instance Monad (ContT r m) where
#if !(MIN_VERSION_base(4,8,0))
    return x = ContT ($ x)
    {-# INLINE return #-}
#endif
    m >>= k  = ContT $ \ c -> runContT m (\ x -> runContT (k x) c)
    {-# INLINE (>>=) #-}

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (ContT r m) where
    fail msg = ContT $ \ _ -> Fail.fail msg
    {-# INLINE fail #-}
#endif

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

--instance MonadCont (ContT r m) where
--    callCC = localCallCC

instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

-- ** delimited continuations **
-- shift = escape
-- reset = capture

data Channel a = Channel
    { _chanId :: !Integer
    , _chanSize :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show)

data ChanAndWaiters chanState r m = ChanAndWaiters
    { _contents :: !(Seq chanState)
    , _readers :: !(Seq (IConcurrentT chanState r m ()))
    , _writers :: !(Seq (IConcurrentT chanState r m ()))
    }

newtype Time = Time DiffTime
    deriving (Show, Eq, Ord, Num, Enum)

newtype Duration = Duration DiffTime
    deriving (Show, Eq, Ord, Num, Enum)

addDuration
    :: Time
    -> Duration
    -> Time
addDuration (Time t) (Duration d) =
    Time (t + d)

subtractTime
    :: Time
    -> Time
    -> Duration
subtractTime (Time end) (Time start) =
    Duration (end - start)

data PriorityCoroutine chanState r m = PriorityCoroutine
    { _routine :: IConcurrentT chanState r m ()
    , _priority :: !Time
    }

-- f *> v = ContT $ \ c -> runContT (ContT $ \ c' -> runContT f (c' . const id)) $ \ g -> runContT v (c . g)

instance Eq (PriorityCoroutine chanState r m)
    where (==) a b =  _priority a == _priority b

instance Ord (PriorityCoroutine chanState r m)
    where compare a b = compare (_priority a) (_priority b)

type CoroutineQueue chanState r m = MinQueue (PriorityCoroutine chanState r m)

data ConcurrentState chanState r m = ConcurrentState
    { _coroutines :: !(CoroutineQueue chanState r m)
    , _scheduledRoutines :: [(Time, IConcurrentT chanState r m ())]
    , _channels :: !(Map (Channel chanState) (ChanAndWaiters chanState r m))
    , _nextChannelIdent :: !Integer
    , _nowTime :: !Time
    }

newtype IConcurrentT chanState r m a =
    IConcurrentT
        { runIConcurrentT' :: ContT r (StateT (ConcurrentState chanState r m) m) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadState (ConcurrentState chanState r m))

instance MonadTrans (IConcurrentT chanState r) where
    lift = IConcurrentT . lift . lift

newtype ConcurrentT chanState r m a =
    ConcurrentT
        { runConcurrentT' :: IConcurrentT chanState r m a
        } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ConcurrentT chanState r) where
    lift = ConcurrentT . IConcurrentT . lift . lift

-- For some reason I had to put these together underneath the definition
-- of `IConcurrentT'
makeLenses ''ConcurrentState
makeLenses ''ChanAndWaiters


freshState
    :: ConcurrentState chanState r m
freshState = ConcurrentState
    { _coroutines = PQueue.empty
    , _scheduledRoutines = []
    , _channels = Data.Map.Strict.empty
    , _nextChannelIdent = 0
    , _nowTime = 0
    }

getCCs
    :: Monad m
    => IConcurrentT chanState r m (CoroutineQueue chanState r m)
getCCs = use coroutines

putCCs
    :: Monad m
    => CoroutineQueue chanState r m
    -> IConcurrentT chanState r m ()
putCCs queue =
    coroutines .= queue

updateNow
    :: Monad m
    => Time
    -> IConcurrentT chanState r m ()
updateNow time =
    nowTime .= time

dequeue
    :: Monad m
    => IConcurrentT chanState r m ()
dequeue = do
    queue <- getCCs
    --scheduled <- use scheduledRoutines
    let mMin = PQueue.minView queue
    case mMin of
        Nothing -> return ()
        Just (PriorityCoroutine nextCoroutine priority, modifiedQueue) -> do
            putCCs modifiedQueue
            updateNow priority
            nextCoroutine
--    case (mMin, scheduled) of
--        (Nothing, []) -> return ()
--        (Just (PriorityCoroutine nextCoroutine priority, modifiedQueue), []) -> do
--            putCCs modifiedQueue
--            updateNow priority
--            nextCoroutine
--        (Nothing, (priority, nextCoroutine): tl) -> do
--            scheduledRoutines .= tl
--            updateNow priority
--            nextCoroutine
--        (Just (PriorityCoroutine nextCoroutineQ priorityQ, modifiedQueue), (priorityL, nextCoroutineL): tl) ->
--            if priorityL <= priorityQ
--            then do
--                scheduledRoutines .= tl
--                updateNow priorityL
--                nextCoroutineL
--            else do
--                putCCs modifiedQueue
--                updateNow priorityQ
--                nextCoroutineQ

ischeduleDuration
    :: Monad m
    => Duration
    -> IConcurrentT chanState r m ()
    -> IConcurrentT chanState r m ()
ischeduleDuration duration routine = do
    currentNow <- inow
    ischedule (addDuration currentNow duration) routine

sleep
    :: Monad m
    => Duration
    -> ConcurrentT r () m ()
sleep = ConcurrentT . isleep

isleep
    :: Monad m
    => Duration
    -> IConcurrentT r () m ()
isleep duration = do
    queue <- getCCs
    if PQueue.null queue
    then do
        currentNow <- inow
        updateNow (addDuration currentNow duration)
    else
        IConcurrentT $ shiftT $ \k -> runIConcurrentT' $ do
            let action = IConcurrentT (lift (k ()))
            ischeduleDuration duration action
            dequeue

yield
    :: Monad m
    => ConcurrentT chanState () m ()
yield = ConcurrentT iyield

iyield
    :: Monad m
    => IConcurrentT chanState () m ()
iyield =
    -- rather than implementing a separate queue/seq for yield'ers, we actually
    -- do want to advance our clock as we yield, simulating CPU cycles
    isleep microsecond

schedule
    :: Monad m
    => Time
    -> ConcurrentT chanState r m ()
    -> ConcurrentT chanState r m ()
schedule time (ConcurrentT f) =
    ConcurrentT (ischedule time f)

lazySchedule
    :: Monad m
    => [(Time, ConcurrentT chanState r m ())]
    -> ConcurrentT chanState r m ()
lazySchedule scheduled =
    ConcurrentT (ilazySchedule [(time, runConcurrentT' t) | (time, t) <- scheduled])

ilazySchedule
    :: Monad m
    => [(Time, IConcurrentT chanState r m ())]
    -> IConcurrentT chanState r m ()
ilazySchedule scheduled =
    scheduledRoutines .= scheduled


ischedule
    :: Monad m
    => Time
    -> IConcurrentT chanState r m ()
    -> IConcurrentT chanState r m ()
ischedule time routine = do
    currentRoutines <- getCCs
    currentNow <- inow
    -- to prevent time from moving backward by scheduling something in the
    -- past, we schedule it to the `max' of the current time, or the schedule
    -- time. Effectively this immediately schedules the process if it were
    -- to otherwise have been scheduled for the past.
    let scheduleTime = max time currentNow
        newRoutines = insertBehind (PriorityCoroutine routine scheduleTime) currentRoutines
    putCCs newRoutines

now
    :: Monad m
    => ConcurrentT chanState r m Time
now = ConcurrentT inow

inow
    :: Monad m
    => IConcurrentT chanState r m Time
inow = use nowTime

fork
    :: Monad m
    => ConcurrentT chanState () m ()
    -> ConcurrentT chanState () m ()
fork (ConcurrentT f) =
    ConcurrentT (ifork f)

ifork
    :: Monad m
    => IConcurrentT chanState () m ()
    -> IConcurrentT chanState () m ()
ifork routine =
    IConcurrentT $ shiftT $ \k -> runIConcurrentT' $ do
        ischeduleDuration 0 (IConcurrentT (lift (k ())))
        routine
        dequeue

newChannel
    :: Monad m
    => Maybe Int
    -> ConcurrentT chanState r m (Channel chanState)
newChannel = ConcurrentT . inewChannel

inewChannel
    :: Monad m
    => Maybe Int
    -> IConcurrentT chanState r m (Channel chanState)
inewChannel mChanSize = do
    -- grab the next channel identifier and then
    -- immediately increment it for the next use
    chanIdent <- use nextChannelIdent
    nextChannelIdent += 1

    let chan = Channel chanIdent mChanSize
        emptySeq = Data.Sequence.empty
        chanAndWaiters = ChanAndWaiters emptySeq emptySeq emptySeq
    channels %= (at chan ?~ chanAndWaiters)
    return chan

writeChannel
    :: Monad m
    => Channel chanState
    -> chanState
    -> ConcurrentT chanState () m ()
writeChannel chan item =
    ConcurrentT (iwriteChannel chan item)

iwriteChannel
    :: Monad m
    => Channel chanState
    -> chanState
    -> IConcurrentT chanState () m ()
iwriteChannel chan@(Channel _ident mMaxSize) item = do
    chanMap <- use channels
    let chanContents = chanMap ^? (ix chan . contents)
        chanCurrentSize = maybe 0 Data.Sequence.length chanContents

    -- when there's already an element, we block and wait our turn to write
    -- once the queue is empty/writable
    case mMaxSize of
        Just maxSize | chanCurrentSize >= maxSize ->
            IConcurrentT $ shiftT $ \k -> runIConcurrentT' $ do
                -- this is a bit dense...:
                -- we add `k ()' to the list of writers, so that
                -- once someone else finds us at the top of the writing queue,
                -- we get rescheduled
                channels . ix chan . writers %= (|> IConcurrentT (lift (k ())))
                dequeue
        _ ->
            return ()


    -- now we've waited, if needed
    -- write the value, and then notify any readers
    -- our state may have changed, so get it again

    -- write the value to the queue
    channels . ix chan . contents %= (|> item)

    chanMap2 <- use channels
    let readerView = fromMaybe EmptyL ((viewl . _readers) <$> Data.Map.Strict.lookup chan chanMap2)
    case readerView of
        -- there are no readers
        EmptyL ->
            dequeue
        -- there is a reader, call the reader
        nextReader :< newReaders -> do
            channels . ix chan . readers .= newReaders
            nextReader

readChannel
    :: Monad m
    => Channel chanState
    -> ConcurrentT chanState () m chanState
readChannel = ConcurrentT . ireadChannel

ireadChannel
    :: Monad m
    => Channel chanState
    -> IConcurrentT chanState () m chanState
ireadChannel chan = do
    chanMap <- use channels
    let mChanContents = fromMaybe EmptyL $ chanMap ^? (ix chan . contents . to viewl)

    case mChanContents of
        EmptyL -> do
            -- nothing to read, so we add ourselves to the queue
            IConcurrentT $ shiftT $ \k -> runIConcurrentT' $ do
                channels . ix chan . readers %= (|> IConcurrentT (lift (k ())))
                dequeue
            -- we can actually just recur here to read the value, since now
            -- that we're running again, the queue will have a value for us to
            -- read
            ireadChannel chan
        val :< newSeq -> do
            -- write the new seq
            channels . ix chan . contents .= newSeq

            -- see if there are any writers
            chanMap2 <- use channels
            let writerView = fromMaybe EmptyL ((viewl . _writers) <$> Data.Map.Strict.lookup chan chanMap2)
            case writerView of
                EmptyL ->
                    return val
                nextWriter :< newWriters -> do
                    channels . ix chan . writers .= newWriters
                    nextWriter
                    return val

runConcurrentT
    :: Monad m
    => ConcurrentT chanState r m r
    -> m r
runConcurrentT (ConcurrentT routine) =
    runIConcurrentT routine

runIConcurrentT
    :: Monad m
    => IConcurrentT chanState r m r
    -> m r
runIConcurrentT routine =
    flip evalStateT freshState $ runContT (resetT $ runIConcurrentT' (routine <* dequeue)) return

microsecond :: Duration
microsecond = Duration (picosecondsToDiffTime 1000000)
