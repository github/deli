{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Concurrent
    ( Channel
    , Time(..)
    , Duration(..)
    , ThreadId
    , ConcurrentT
    , addDuration
    , microsecond
    , millisecond
    , millisecondsToDuration
    , millisecondsToTime
    , microsecondsToDuration
    , microsecondsToTime
    , subtractTime
    , fork
    , threadId
    , sleep
    , yield
    , lazySchedule
    , now
    , newChannel
    , writeChannel
    , writeChannelNonblocking
    , readChannel
    , readChannelNonblocking
    , runConcurrentT
    ) where

import Control.Lens (at, ix, makeLenses, to, use, (^?), (.=), (+=), (%=), (?~))
import Control.Monad.State.Strict
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.Cont (ContT, evalContT, resetT, shiftT)
import Data.Map.Strict
import Data.Maybe
import Data.PQueue.Min as PQueue
import Data.Sequence
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)

data Queue a = Queue
    { _writeEnd :: [a]
    , _readEnd :: [a]
    }

emptyQueue :: Queue a
emptyQueue = Queue [] []

readQueue
    :: Queue a
    -> Maybe (a, Queue a)
readQueue (Queue writeEnd readEnd) =
    case readEnd of
        (h:tl) ->
            let newQueue = Queue writeEnd tl
            in Just (h, newQueue)
        [] ->
            if Prelude.null writeEnd
            then Nothing
            else readQueue (Queue [] (Prelude.reverse writeEnd))

writeQueue
    :: Queue a
    -> a
    -> Queue a
writeQueue (Queue writeEnd readEnd) val =
    Queue (val:writeEnd) readEnd

-- ** delimited continuations **
-- shift = escape
-- reset = capture

data Channel a = Channel
    { _chanId :: !Integer
    , _chanSize :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show)

data ChanAndWaiters chanState m = ChanAndWaiters
    { _contents :: !(Seq chanState)
    , _readers :: Queue (ThreadId, IConcurrentT chanState m ())
    , _writers :: Queue (ThreadId, IConcurrentT chanState m ())
    }

newtype Time = Time DiffTime
    deriving (Show, Eq, Ord, Num, Fractional, Enum)

newtype Duration = Duration DiffTime
    deriving (Show, Eq, Ord, Num, Fractional, Real, Enum)

newtype ThreadId = ThreadId Integer
    deriving (Show, Eq, Ord)

addDuration
    :: Time
    -> Duration
    -> Time
addDuration (Time t) (Duration d) =
    Time (t + d)

microsecond :: Duration
microsecond = Duration (picosecondsToDiffTime 1000000)

millisecond :: Duration
millisecond = microsecond * 1000

millisecondsToTime
    :: Integer
    -> Time
millisecondsToTime millis =
    Time $ picosecondsToDiffTime (1000 * 1000 * 1000 * millis)

millisecondsToDuration
    :: Integer
    -> Duration
millisecondsToDuration millis =
    Duration $ picosecondsToDiffTime (1000 * 1000 * 1000 * millis)

microsecondsToTime
    :: Integer
    -> Time
microsecondsToTime micros =
    Time $ picosecondsToDiffTime (1000 * 1000 * micros)

microsecondsToDuration
    :: Integer
    -> Duration
microsecondsToDuration micros =
    Duration $ picosecondsToDiffTime (1000 * 1000 * micros)

subtractTime
    :: Time
    -> Time
    -> Duration
subtractTime (Time end) (Time start) =
    Duration (end - start)

data PriorityCoroutine chanState m = PriorityCoroutine
    { _routine :: IConcurrentT chanState m ()
    , _pId :: !ThreadId
    , _priority :: !Time
    }

instance Eq (PriorityCoroutine chanState m)
    where (==) a b =  (_priority a, _pId a) == (_priority b, _pId b)

instance Ord (PriorityCoroutine chanState m)
    -- NOTE: should this incorporate the threadId?
    where compare a b = compare (_priority a) (_priority b)

type CoroutineQueue chanState m = MinQueue (PriorityCoroutine chanState m)

data ConcurrentState chanState m = ConcurrentState
    { _coroutines :: !(CoroutineQueue chanState m)
    , _scheduledRoutines :: [(Time, IConcurrentT chanState m ())]
    , _nextThreadIdent :: !ThreadId
    , _channels :: !(Map (Channel chanState) (ChanAndWaiters chanState m))
    , _nextChannelIdent :: !Integer
    , _nowTime :: !Time
    }

newtype IConcurrentT chanState m a =
    IConcurrentT
        { runIConcurrentT' :: ContT () (ReaderT ThreadId (StateT (ConcurrentState chanState m) m)) a
        } deriving (Functor, Monad, MonadIO, MonadReader ThreadId, MonadState (ConcurrentState chanState m))

instance Applicative (IConcurrentT chanState m) where
    pure = IConcurrentT . pure

    (IConcurrentT a) <*> (IConcurrentT b) = IConcurrentT (a <*> b)

    (IConcurrentT a) *> (IConcurrentT b) = IConcurrentT $ a >>= const b

instance MonadTrans (IConcurrentT chanState) where
    lift = IConcurrentT . lift . lift . lift

newtype ConcurrentT chanState m a =
    ConcurrentT
        { runConcurrentT' :: IConcurrentT chanState m a
        } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState s m => MonadState s (ConcurrentT chanState m) where
    get = lift get

    put = lift . put

    state = lift . state

instance MonadTrans (ConcurrentT chanState) where
    lift = ConcurrentT . IConcurrentT . lift . lift . lift

-- For some reason I had to put these together underneath the definition
-- of `IConcurrentT'
makeLenses ''ConcurrentState
makeLenses ''ChanAndWaiters


freshState
    :: ConcurrentState chanState m
freshState = ConcurrentState
    { _coroutines = PQueue.empty
    , _scheduledRoutines = []
    -- 1 because we `runReaderT' with 0 for the main thread,
    -- which is already created
    , _nextThreadIdent = ThreadId 1
    , _channels = Data.Map.Strict.empty
    , _nextChannelIdent = 0
    , _nowTime = 0
    }

register
    :: Monad m
    => (IConcurrentT chanState m () -> IConcurrentT chanState m ())
    -> IConcurrentT chanState m ()
register callback =
    IConcurrentT $ shiftT $ \k -> do
        let routine = IConcurrentT (lift (k ()))
        runIConcurrentT' (callback routine)

getCCs
    :: Monad m
    => IConcurrentT chanState m (CoroutineQueue chanState m)
getCCs = use coroutines

putCCs
    :: Monad m
    => CoroutineQueue chanState m
    -> IConcurrentT chanState m ()
putCCs queue =
    coroutines .= queue

updateNow
    :: Monad m
    => Time
    -> IConcurrentT chanState m ()
updateNow time =
    nowTime .= time

dequeue
    :: Monad m
    => IConcurrentT chanState m ()
dequeue = do
    queue <- getCCs
    scheduled <- use scheduledRoutines
    let mMin = PQueue.minView queue
    case (mMin, scheduled) of
        (Nothing, []) ->
            return ()
        (Just (PriorityCoroutine nextCoroutine pId priority, modifiedQueue), []) -> do
            putCCs modifiedQueue
            updateNow priority
            IConcurrentT (resetT (runIConcurrentT' (local (const pId) nextCoroutine)))
            dequeue
        (Nothing, (priority, nextCoroutine): tl) -> do
            scheduledRoutines .= tl
            updateNow priority
            IConcurrentT (resetT (runIConcurrentT' nextCoroutine))
            dequeue
        (Just (PriorityCoroutine nextCoroutineQ pId priorityQ, modifiedQueue), (priorityL, nextCoroutineL): tl) ->
            if priorityL <= priorityQ
            then do
                scheduledRoutines .= tl
                updateNow priorityL
                IConcurrentT (resetT (runIConcurrentT' (local (const pId) nextCoroutineL)))
                dequeue
            else do
                putCCs modifiedQueue
                updateNow priorityQ
                IConcurrentT (resetT (runIConcurrentT' nextCoroutineQ))
                dequeue

ischeduleDuration
    :: Monad m
    => Duration
    -> ThreadId
    -> IConcurrentT chanState m ()
    -> IConcurrentT chanState m ()
ischeduleDuration duration pId routine = do
    currentNow <- inow
    ischedule (addDuration currentNow duration) pId routine

sleep
    :: Monad m
    => Duration
    -> ConcurrentT chanState m ()
sleep = ConcurrentT . isleep

isleep
    :: Monad m
    => Duration
    -> IConcurrentT chanState m ()
isleep duration = do
    myId <- ithreadId
    register (ischeduleDuration duration myId)

yield
    :: Monad m
    => ConcurrentT chanState m ()
yield = ConcurrentT iyield

iyield
    :: Monad m
    => IConcurrentT chanState m ()
iyield =
    -- rather than implementing a separate queue/seq for yield'ers, we actually
    -- do want to advance our clock as we yield, simulating CPU cycles
    isleep microsecond

lazySchedule
    :: Monad m
    => [(Time, ConcurrentT chanState m ())]
    -> ConcurrentT chanState m ()
lazySchedule scheduled =
    ConcurrentT (ilazySchedule [(time, runConcurrentT' t) | (time, t) <- scheduled])

ilazySchedule
    :: Monad m
    => [(Time, IConcurrentT chanState m ())]
    -> IConcurrentT chanState m ()
ilazySchedule scheduled =
    scheduledRoutines .= scheduled


ischedule
    :: Monad m
    => Time
    -> ThreadId
    -> IConcurrentT chanState m ()
    -> IConcurrentT chanState m ()
ischedule time pId routine = do
    currentRoutines <- getCCs
    currentNow <- inow
    -- to prevent time from moving backward by scheduling something in the
    -- past, we schedule it to the `max' of the current time, or the schedule
    -- time. Effectively this immediately schedules the process if it were
    -- to otherwise have been scheduled for the past.
    let scheduleTime = max time currentNow
        newRoutines = insertBehind (PriorityCoroutine routine pId scheduleTime) currentRoutines
    putCCs newRoutines

now
    :: Monad m
    => ConcurrentT chanState m Time
now = ConcurrentT inow

inow
    :: Monad m
    => IConcurrentT chanState m Time
inow = use nowTime

fork
    :: Monad m
    => ConcurrentT chanState m ()
    -> ConcurrentT chanState m ()
fork (ConcurrentT f) =
    ConcurrentT (ifork f)

ifork
    :: Monad m
    => IConcurrentT chanState m ()
    -> IConcurrentT chanState m ()
ifork routine = do
    tId@(ThreadId i) <- use nextThreadIdent
    nextThreadIdent .= (ThreadId (i + 1))
    ischeduleDuration 0 tId routine
    myId <- ithreadId
    register (ischeduleDuration 0 myId)

threadId
    :: Monad m
    => ConcurrentT chanState m ThreadId
threadId = ConcurrentT ithreadId

ithreadId
    :: Monad m
    => IConcurrentT chanState m ThreadId
ithreadId = ask

newChannel
    :: Monad m
    => Maybe Int
    -> ConcurrentT chanState m (Channel chanState)
newChannel = ConcurrentT . inewChannel

inewChannel
    :: Monad m
    => Maybe Int
    -> IConcurrentT chanState m (Channel chanState)
inewChannel mChanSize = do
    -- grab the next channel identifier and then
    -- immediately increment it for the next use
    chanIdent <- use nextChannelIdent
    nextChannelIdent += 1

    let chan = Channel chanIdent mChanSize
        emptySeq = Data.Sequence.empty
        chanAndWaiters = ChanAndWaiters emptySeq emptyQueue emptyQueue
    channels %= (at chan ?~ chanAndWaiters)
    return chan

writeChannel
    :: Monad m
    => Channel chanState
    -> chanState
    -> ConcurrentT chanState m ()
writeChannel chan item =
    ConcurrentT (iwriteChannel chan item)

iwriteChannel
    :: Monad m
    => Channel chanState
    -> chanState
    -> IConcurrentT chanState m ()
iwriteChannel chan@(Channel _ident mMaxSize) item = do
    chanMap <- use channels
    let chanContents = chanMap ^? (ix chan . contents)
        chanCurrentSize = maybe 0 Data.Sequence.length chanContents

    myId <- ithreadId

    -- when there's already an element, we block and wait our turn to write
    -- once the queue is empty/writable
    case mMaxSize of
        Just maxSize | chanCurrentSize >= maxSize ->
            register $ \routine ->
                channels . ix chan . writers %= flip writeQueue (myId, routine)
        _ ->
            return ()


    -- now we've waited, if needed
    -- write the value, and then notify any readers
    -- our state may have changed, so get it again

    -- write the value to the queue
    channels . ix chan . contents %= (|> item)

    chanMap2 <- use channels
    let readerView = join $ (readQueue . _readers) <$> Data.Map.Strict.lookup chan chanMap2
    case readerView of
        -- there are no readers
        Nothing ->
            return ()
        -- there is a reader, call the reader
        Just ((readerId, nextReader), newReaders) -> do
            channels . ix chan . readers .= newReaders
            local (const readerId) nextReader

writeChannelNonblocking
    :: Monad m
    => Channel chanState
    -> chanState
    -> ConcurrentT chanState m (Maybe chanState)
writeChannelNonblocking chan item =
    ConcurrentT (iwriteChannelNonblocking chan item)

iwriteChannelNonblocking
    :: Monad m
    => Channel chanState
    -> chanState
    -> IConcurrentT chanState m (Maybe chanState)
iwriteChannelNonblocking chan@(Channel _ident mMaxSize) item = do
    chanMap <- use channels
    myId <- ithreadId
    let chanContents = chanMap ^? (ix chan . contents)
        chanCurrentSize = maybe 0 Data.Sequence.length chanContents

    -- when there's already an element, we block and wait our turn to write
    -- once the queue is empty/writable
    case mMaxSize of
        Just maxSize | chanCurrentSize >= maxSize ->
            return Nothing
        _ -> do
            -- write the value to the queue
            channels . ix chan . contents %= (|> item)

            chanMap2 <- use channels
            let readerView = join $ (readQueue . _readers) <$> Data.Map.Strict.lookup chan chanMap2
            case readerView of
                -- there are no readers
                Nothing ->
                    return (Just item)
                -- there is a reader, call the reader
                Just ((readerId, nextReader), newReaders) -> do
                    channels . ix chan . readers .= newReaders
                    --local (const readerId) nextReader
                    ischeduleDuration 0 readerId nextReader
                    register (ischeduleDuration 0 myId)
                    return (Just item)

readChannel
    :: Monad m
    => Channel chanState
    -> ConcurrentT chanState m chanState
readChannel = ConcurrentT . ireadChannel

ireadChannel
    :: Monad m
    => Channel chanState
    -> IConcurrentT chanState m chanState
ireadChannel chan = do
    chanMap <- use channels
    let mChanContents = fromMaybe EmptyL $ chanMap ^? (ix chan . contents . to viewl)

    myId <- ithreadId

    case mChanContents of
        EmptyL -> do
            -- nothing to read, so we add ourselves to the queue
            register $ \routine ->
                channels . ix chan . readers %= flip writeQueue (myId, routine)
            -- we can actually just recur here to read the value, since now
            -- that we're running again, the queue will have a value for us to
            -- read
            ireadChannel chan
        val :< newSeq -> do
            -- write the new seq
            channels . ix chan . contents .= newSeq

            -- see if there are any writers
            chanMap2 <- use channels
            let writerView = join $ (readQueue . _writers) <$> Data.Map.Strict.lookup chan chanMap2
            case writerView of
                Nothing ->
                    return val
                Just ((writerId, nextWriter), newWriters) -> do
                    channels . ix chan . writers .= newWriters
                    local (const writerId) nextWriter
                    return val

readChannelNonblocking
    :: Monad m
    => Channel chanState
    -> ConcurrentT chanState m (Maybe chanState)
readChannelNonblocking = ConcurrentT . ireadChannelNonblocking

ireadChannelNonblocking
    :: Monad m
    => Channel chanState
    -> IConcurrentT chanState m (Maybe chanState)
ireadChannelNonblocking chan = do
    chanMap <- use channels
    let mChanContents = fromMaybe EmptyL $ chanMap ^? (ix chan . contents . to viewl)

    case mChanContents of
        EmptyL -> return Nothing
        val :< newSeq -> do
            -- write the new seq
            channels . ix chan . contents .= newSeq

            -- see if there are any writers
            chanMap2 <- use channels
            let writerView = join $ (readQueue . _writers) <$> Data.Map.Strict.lookup chan chanMap2
            case writerView of
                Nothing ->
                    return (Just val)
                Just ((writerId, nextWriter), newWriters) -> do
                    channels . ix chan . writers .= newWriters
                    local (const writerId) nextWriter
                    return (Just val)

runConcurrentT
    :: Monad m
    => ConcurrentT chanState m ()
    -> m ()
runConcurrentT (ConcurrentT routine) =
    runIConcurrentT routine

runIConcurrentT
    :: Monad m
    => IConcurrentT chanState m ()
    -> m ()
runIConcurrentT routine =
    let resetAction = do
            resetT (runIConcurrentT' routine)
            runIConcurrentT' dequeue
    in
    void $ flip evalStateT freshState $ flip runReaderT (ThreadId 0) $ evalContT resetAction

