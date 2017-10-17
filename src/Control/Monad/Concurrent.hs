{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
    , now
    , newChannel
    , writeChannel
    , readChannel
    , runConcurrentT
    ) where

import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.PQueue.Min
import Control.Lens (at, ix, makeLenses, to, use, (^.), (^?), (.=), (+=), (%=), (?~))
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Trans.Cont (resetT, shiftT)
import Data.Sequence
import Data.Map.Strict
import Data.Maybe

-- ** delimited continuations **
-- shift = escape
-- reset = capture

data Channel a = Channel
    { _chanId :: Integer
    , _chanSize :: Maybe Int
    }
    deriving (Eq, Ord)

data ChanAndWaiters chanState r m = ChanAndWaiters
    { _contents :: Seq chanState
    , _readers :: Seq (IConcurrentT chanState r m ())
    , _writers :: Seq (IConcurrentT chanState r m ())
    }

newtype Time = Time DiffTime
    deriving (Show, Eq, Ord, Num)

newtype Duration = Duration DiffTime
    deriving (Show, Eq, Ord, Num)

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
    { routine :: IConcurrentT chanState r m ()
    , priority :: Time
    }

instance Eq (PriorityCoroutine chanState r m)
    where (==) a b =  priority a == priority b

instance Ord (PriorityCoroutine chanState r m)
    where compare a b = compare (priority a) (priority b)

type CoroutineQueue chanState r m = MinQueue (PriorityCoroutine chanState r m)

data ConcurrentState chanState r m = ConcurrentState
    { _coroutines :: CoroutineQueue chanState r m
    , _channels :: Map (Channel chanState) (ChanAndWaiters chanState r m)
    , _nextChannelIdent :: Integer
    , _nowTime :: Time
    }

newtype IConcurrentT chanState r m a =
    IConcurrentT
        { runIConcurrentT' :: ContT r (StateT (ConcurrentState chanState r m) m) a
        } deriving (Functor, Applicative, Monad, MonadCont, MonadIO, MonadState (ConcurrentState chanState r m))

instance MonadTrans (IConcurrentT chanState r) where
    lift = IConcurrentT . lift . lift

newtype ConcurrentT chanState r m a =
    ConcurrentT
        { runConcurrentT' :: IConcurrentT chanState r m a
        } deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

instance MonadTrans (ConcurrentT chanState r) where
    lift = ConcurrentT . IConcurrentT . lift . lift

-- For some reason I had to put these together underneath the definition
-- of `IConcurrentT'
makeLenses ''ConcurrentState
makeLenses ''ChanAndWaiters


freshState :: ConcurrentState chanState r m
freshState = ConcurrentState
    { _coroutines = Data.PQueue.Min.empty
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
    let mMin = Data.PQueue.Min.minView queue
    case mMin of
        Nothing -> return ()
        Just (PriorityCoroutine nextCoroutine priority, modifiedQueue) -> do
            putCCs modifiedQueue
            updateNow priority
            nextCoroutine

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
    -> ConcurrentT chanState r m ()
sleep = ConcurrentT . isleep

isleep
    :: Monad m
    => Duration
    -> IConcurrentT chanState r m ()
isleep duration =
    callCC $ \k -> do
        ischeduleDuration duration (k ())
        dequeue

yield
    :: Monad m
    => ConcurrentT chanState r m ()
yield = ConcurrentT iyield

iyield
    :: Monad m
    => IConcurrentT chanState r m ()
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
    => ConcurrentT chanState r m ()
    -> ConcurrentT chanState r m ()
fork (ConcurrentT f) =
    ConcurrentT (ifork f)

ifork
    :: Monad m
    => IConcurrentT chanState r m ()
    -> IConcurrentT chanState r m ()
ifork routine =
    callCC $ \k -> do
        ischeduleDuration 0 (k ())
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
-- ignoring queue size for now..., it's just always 1 (one)
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
iwriteChannel chan@(Channel ident mMaxSize) item = do
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

    chanMap <- use channels
    let readerView = fromMaybe EmptyL ((viewl . _readers) <$> Data.Map.Strict.lookup chan chanMap)
    case readerView of
        -- there are no readers
        EmptyL ->
            return ()
        -- there is a reader, call the reader
        nextReader :< newReaders -> do
            channels . ix chan . readers .= newReaders
            nextReader
            -- should we be yielding here at all?

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
            chanMap <- use channels
            let writerView = fromMaybe EmptyL ((viewl . _writers) <$> Data.Map.Strict.lookup chan chanMap)
            case writerView of
                EmptyL ->
                    return val
                nextWriter :< newWriters -> do
                    channels . ix chan . writers .= newWriters
                    nextWriter
                    return val

exhaust
    :: Monad m
    => IConcurrentT chanState r m ()
exhaust = do
    exhausted <- Data.PQueue.Min.null <$> getCCs
    unless exhausted $
        isleep 1000 >> exhaust

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
