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
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable)
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

data UntypedChannel = UntypedChannel
    { _untypedChanId :: !Integer
    , _untypedChanSize :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show)

chanToUntypedChan
    :: Channel a
    -> UntypedChannel
chanToUntypedChan (Channel ident chanSize) =
    UntypedChannel ident chanSize

data ChanAndWaiters m = ChanAndWaiters
    { _contents :: !(Seq Dynamic)
    , _readers :: Queue (ThreadId, IConcurrentT m ())
    , _writers :: Queue (ThreadId, IConcurrentT m ())
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

data PriorityCoroutine m = PriorityCoroutine
    { _routine :: IConcurrentT m ()
    , _pId :: !ThreadId
    , _priority :: !Time
    }

instance Eq (PriorityCoroutine m)
    where (==) a b =  (_priority a, _pId a) == (_priority b, _pId b)

instance Ord (PriorityCoroutine m)
    -- NOTE: should this incorporate the threadId?
    where compare a b = compare (_priority a) (_priority b)

type CoroutineQueue m = MinQueue (PriorityCoroutine m)

data ConcurrentState m = ConcurrentState
    { _coroutines :: !(CoroutineQueue m)
    , _scheduledRoutines :: [(Time, IConcurrentT m ())]
    , _nextThreadIdent :: !ThreadId
    , _channels :: !(Map UntypedChannel (ChanAndWaiters m))
    , _nextChannelIdent :: !Integer
    , _nowTime :: !Time
    }

newtype IConcurrentT m a =
    IConcurrentT
        { runIConcurrentT' :: ContT () (ReaderT ThreadId (StateT (ConcurrentState m) m)) a
        } deriving (Functor, Monad, MonadIO, MonadReader ThreadId, MonadState (ConcurrentState m))

instance Applicative (IConcurrentT m) where
    pure = IConcurrentT . pure

    (IConcurrentT a) <*> (IConcurrentT b) = IConcurrentT (a <*> b)

    (IConcurrentT a) *> (IConcurrentT b) = IConcurrentT $ a >>= const b

instance MonadTrans IConcurrentT where
    lift = IConcurrentT . lift . lift . lift

newtype ConcurrentT m a =
    ConcurrentT
        { runConcurrentT' :: IConcurrentT m a
        } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState s m => MonadState s (ConcurrentT m) where
    get = lift get

    put = lift . put

    state = lift . state

instance MonadTrans ConcurrentT where
    lift = ConcurrentT . IConcurrentT . lift . lift . lift

-- For some reason I had to put these together underneath the definition
-- of `IConcurrentT'
makeLenses ''ConcurrentState
makeLenses ''ChanAndWaiters


freshState
    :: ConcurrentState m
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
    => (IConcurrentT m () -> IConcurrentT m ())
    -> IConcurrentT m ()
register callback =
    IConcurrentT $ shiftT $ \k -> do
        let routine = IConcurrentT (lift (k ()))
        runIConcurrentT' (callback routine)

getCCs
    :: Monad m
    => IConcurrentT m (CoroutineQueue m)
getCCs = use coroutines

putCCs
    :: Monad m
    => CoroutineQueue m
    -> IConcurrentT m ()
putCCs queue =
    coroutines .= queue

updateNow
    :: Monad m
    => Time
    -> IConcurrentT m ()
updateNow time =
    nowTime .= time

dequeue
    :: Monad m
    => IConcurrentT m ()
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
    -> IConcurrentT m ()
    -> IConcurrentT m ()
ischeduleDuration duration pId routine = do
    currentNow <- inow
    ischedule (addDuration currentNow duration) pId routine

sleep
    :: Monad m
    => Duration
    -> ConcurrentT m ()
sleep = ConcurrentT . isleep

isleep
    :: Monad m
    => Duration
    -> IConcurrentT m ()
isleep duration = do
    myId <- ithreadId
    register (ischeduleDuration duration myId)

yield
    :: Monad m
    => ConcurrentT m ()
yield = ConcurrentT iyield

iyield
    :: Monad m
    => IConcurrentT m ()
iyield =
    -- rather than implementing a separate queue/seq for yield'ers, we actually
    -- do want to advance our clock as we yield, simulating CPU cycles
    isleep microsecond

lazySchedule
    :: Monad m
    => [(Time, ConcurrentT m ())]
    -> ConcurrentT m ()
lazySchedule scheduled =
    ConcurrentT (ilazySchedule [(time, runConcurrentT' t) | (time, t) <- scheduled])

ilazySchedule
    :: Monad m
    => [(Time, IConcurrentT m ())]
    -> IConcurrentT m ()
ilazySchedule scheduled =
    scheduledRoutines .= scheduled


ischedule
    :: Monad m
    => Time
    -> ThreadId
    -> IConcurrentT m ()
    -> IConcurrentT m ()
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
    => ConcurrentT m Time
now = ConcurrentT inow

inow
    :: Monad m
    => IConcurrentT m Time
inow = use nowTime

fork
    :: Monad m
    => ConcurrentT m ()
    -> ConcurrentT m ()
fork (ConcurrentT f) =
    ConcurrentT (ifork f)

ifork
    :: Monad m
    => IConcurrentT m ()
    -> IConcurrentT m ()
ifork routine = do
    tId@(ThreadId i) <- use nextThreadIdent
    nextThreadIdent .= (ThreadId (i + 1))
    ischeduleDuration 0 tId routine
    myId <- ithreadId
    register (ischeduleDuration 0 myId)

threadId
    :: Monad m
    => ConcurrentT m ThreadId
threadId = ConcurrentT ithreadId

ithreadId
    :: Monad m
    => IConcurrentT m ThreadId
ithreadId = ask

newChannel
    :: Monad m
    => Maybe Int
    -> ConcurrentT m (Channel chanState)
newChannel = ConcurrentT . inewChannel

inewChannel
    :: Monad m
    => Maybe Int
    -> IConcurrentT m (Channel chanState)
inewChannel mChanSize = do
    -- grab the next channel identifier and then
    -- immediately increment it for the next use
    chanIdent <- use nextChannelIdent
    nextChannelIdent += 1

    let chan = Channel chanIdent mChanSize
        untypedChan = UntypedChannel chanIdent mChanSize
        emptySeq = Data.Sequence.empty
        chanAndWaiters = ChanAndWaiters emptySeq emptyQueue emptyQueue
    channels %= (at untypedChan ?~ chanAndWaiters)
    return chan

writeChannel
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> chanState
    -> ConcurrentT m ()
writeChannel chan item =
    ConcurrentT (iwriteChannel chan item)

iwriteChannel
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> chanState
    -> IConcurrentT m ()
iwriteChannel chan@(Channel _ident mMaxSize) item = do
    chanMap <- use channels
    let untypedChan = chanToUntypedChan chan
    let chanContents = chanMap ^? (ix untypedChan . contents)
        chanCurrentSize = maybe 0 Data.Sequence.length chanContents

    myId <- ithreadId

    -- when there's already an element, we block and wait our turn to write
    -- once the queue is empty/writable
    case mMaxSize of
        Just maxSize | chanCurrentSize >= maxSize ->
            register $ \routine ->
                channels . ix untypedChan . writers %= flip writeQueue (myId, routine)
        _ ->
            return ()


    -- now we've waited, if needed
    -- write the value, and then notify any readers
    -- our state may have changed, so get it again

    -- write the value to the queue
    channels . ix untypedChan . contents %= (|> toDyn item)

    chanMap2 <- use channels
    let readerView = join $ (readQueue . _readers) <$> Data.Map.Strict.lookup untypedChan chanMap2
    case readerView of
        -- there are no readers
        Nothing ->
            return ()
        -- there is a reader, call the reader
        Just ((readerId, nextReader), newReaders) -> do
            channels . ix untypedChan . readers .= newReaders
            local (const readerId) nextReader

writeChannelNonblocking
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> chanState
    -> ConcurrentT m (Maybe chanState)
writeChannelNonblocking chan item =
    ConcurrentT (iwriteChannelNonblocking chan item)

iwriteChannelNonblocking
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> chanState
    -> IConcurrentT m (Maybe chanState)
iwriteChannelNonblocking chan@(Channel _ident mMaxSize) item = do
    let untypedChan = chanToUntypedChan chan
    chanMap <- use channels
    myId <- ithreadId
    let chanContents = chanMap ^? (ix untypedChan . contents)
        chanCurrentSize = maybe 0 Data.Sequence.length chanContents

    -- when there's already an element, we block and wait our turn to write
    -- once the queue is empty/writable
    case mMaxSize of
        Just maxSize | chanCurrentSize >= maxSize ->
            return Nothing
        _ -> do
            -- write the value to the queue
            channels . ix untypedChan . contents %= (|> toDyn item)

            chanMap2 <- use channels
            let readerView = join $ (readQueue . _readers) <$> Data.Map.Strict.lookup untypedChan chanMap2
            case readerView of
                -- there are no readers
                Nothing ->
                    return (Just item)
                -- there is a reader, call the reader
                Just ((readerId, nextReader), newReaders) -> do
                    channels . ix untypedChan . readers .= newReaders
                    --local (const readerId) nextReader
                    ischeduleDuration 0 readerId nextReader
                    register (ischeduleDuration 0 myId)
                    return (Just item)

readChannel
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> ConcurrentT m chanState
readChannel = ConcurrentT . ireadChannel

ireadChannel
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> IConcurrentT m chanState
ireadChannel chan = do
    chanMap <- use channels
    let untypedChan = chanToUntypedChan chan
        mChanContents = fromMaybe EmptyL $ chanMap ^? (ix untypedChan . contents . to viewl)

    myId <- ithreadId

    case mChanContents of
        EmptyL -> do
            -- nothing to read, so we add ourselves to the queue
            register $ \routine ->
                channels . ix untypedChan . readers %= flip writeQueue (myId, routine)
            -- we can actually just recur here to read the value, since now
            -- that we're running again, the queue will have a value for us to
            -- read
            ireadChannel chan
        dVal :< newSeq ->
            case fromDynamic dVal of
                Nothing -> error "Dynamic channels programming error"
                Just val -> do
                    -- write the new seq
                    channels . ix untypedChan . contents .= newSeq

                    -- see if there are any writers
                    chanMap2 <- use channels
                    let writerView = join $ (readQueue . _writers) <$> Data.Map.Strict.lookup untypedChan chanMap2
                    case writerView of
                        Nothing ->
                            return val
                        Just ((writerId, nextWriter), newWriters) -> do
                            channels . ix untypedChan . writers .= newWriters
                            local (const writerId) nextWriter
                            return val

readChannelNonblocking
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> ConcurrentT m (Maybe chanState)
readChannelNonblocking = ConcurrentT . ireadChannelNonblocking

ireadChannelNonblocking
    :: (Monad m, Typeable chanState)
    => Channel chanState
    -> IConcurrentT m (Maybe chanState)
ireadChannelNonblocking chan = do
    chanMap <- use channels
    let untypedChan = chanToUntypedChan chan
        mChanContents = fromMaybe EmptyL $ chanMap ^? (ix untypedChan . contents . to viewl)

    case mChanContents of
        EmptyL -> return Nothing
        dVal :< newSeq -> do
            -- write the new seq
            channels . ix untypedChan . contents .= newSeq

            -- see if there are any writers
            chanMap2 <- use channels
            let writerView = join $ (readQueue . _writers) <$> Data.Map.Strict.lookup untypedChan chanMap2
            case writerView of
                Nothing ->
                    return (fromDynamic dVal)
                Just ((writerId, nextWriter), newWriters) -> do
                    channels . ix untypedChan . writers .= newWriters
                    local (const writerId) nextWriter
                    return (fromDynamic dVal)

runConcurrentT
    :: Monad m
    => ConcurrentT m ()
    -> m ()
runConcurrentT (ConcurrentT routine) =
    runIConcurrentT routine

runIConcurrentT
    :: Monad m
    => IConcurrentT m ()
    -> m ()
runIConcurrentT routine =
    let resetAction = do
            resetT (runIConcurrentT' routine)
            runIConcurrentT' dequeue
    in
    void $ flip evalStateT freshState $ flip runReaderT (ThreadId 0) $ evalContT resetAction

