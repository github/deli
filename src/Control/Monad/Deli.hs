{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Deli
    ( Deli
    , Job(..)
    , TimesliceStats(..)
    , DeliState(..)
    -- re-exported from Control.Monad.Concurrent
    , Concurrent.Time(..)
    , Concurrent.Duration
    , Concurrent.Channel
    , Concurrent.microsecond
    , Concurrent.millisecond
    , Concurrent.millisecondsToDuration
    , fork
    , sleep
    , now
    , newChannel
    , writeChannel
    , readChannel
    , runDeli
    , runJob
    , simulate
    ) where

import Control.Lens (makeLenses, use, (%~), (+~), (.~), (^.))
--import Control.Monad.Random.Strict
import Control.Monad.State.Strict (State, execState, modify')
import Data.Function ((&))
import Data.Map.Strict
import Data.Maybe (fromJust)
import Data.TDigest (TDigest, tdigest, quantile)
import Data.Time
import System.Random (StdGen)
import qualified Control.Monad.Concurrent as Concurrent
import qualified Data.TDigest as TDigest

data Job = Job
    { _jobStart :: !Concurrent.Time
    , _jobDuration :: !Concurrent.Duration
    } deriving (Show, Eq, Ord)

data FinishedJob = FinishedJob
    { _jobFinishTime :: Concurrent.Time
    , _jobSojourn :: Concurrent.Duration
    } deriving (Show, Eq, Ord)

data TimesliceStats = TimesliceStats
    {
    -- inclusive
      _sliceStart :: Concurrent.Time
    , _response50 :: Concurrent.Duration
    , _response99 :: Concurrent.Duration
    } deriving (Show)

data DeliState = DeliState
    { _sojournStatistics :: !(TDigest 10)
    , _perfectStatistics :: !(TDigest 10)
    , _temporalStats :: !(Map Concurrent.Time TimesliceStats)
    , _currentMinute :: !Concurrent.Time
    , _currentDigest :: !(TDigest 10)
    , _numProcessed :: !Integer
    } deriving (Show)

makeLenses ''DeliState

freshState :: DeliState
freshState =
    DeliState
        { _sojournStatistics = emptyDigest
        , _perfectStatistics = emptyDigest
        , _temporalStats = Data.Map.Strict.empty
        , _currentMinute = 0
        , _currentDigest = emptyDigest
        , _numProcessed = 0
        }
    where emptyDigest = tdigest []

newtype Deli chanState a =
    Deli
        { _getDeli :: Concurrent.ConcurrentT chanState (State DeliState) a
        } deriving (Functor, Applicative, Monad)

--instance MonadRandom (Deli chanState) where
--    getRandomR range = getRandomR range >>= Deli . pure
--
--    getRandom = getRandom >>= Deli . pure
--
--    getRandomRs range = getRandomRs range >>= Deli . pure
--
--    getRandoms = getRandoms >>= Deli . pure

------------------------------------------------------------------------------
-- ## Wrappers around the Control.Monad.Concurrent API
------------------------------------------------------------------------------

fork
    :: Deli chanState ()
    -> Deli chanState ()
fork (Deli conc) =
    Deli $ Concurrent.fork conc

sleep
    :: Concurrent.Duration
    -> Deli chanState ()
sleep = Deli . Concurrent.sleep

now
    :: Deli chanState Concurrent.Time
now = Deli Concurrent.now

newChannel
    :: Maybe Int
    -> Deli chanState (Concurrent.Channel chanState)
newChannel = Deli . Concurrent.newChannel

writeChannel
    :: Concurrent.Channel chanState
    -> chanState
    -> Deli chanState ()
writeChannel chan item =
    Deli (Concurrent.writeChannel chan item)

readChannel
    :: Concurrent.Channel chanState
    -> Deli chanState chanState
readChannel = Deli . Concurrent.readChannel

------------------------------------------------------------------------------
-- ## Time Conversion
------------------------------------------------------------------------------

-- Round down a `Concurrent.Time' to the nearest minute
clampMinutes
    :: Concurrent.Time
    -> Concurrent.Time
clampMinutes (Concurrent.Time t) =
    let picosPerMinute = 60000000000000
        inPicos = diffTimeToPicoseconds t
        toMinute = inPicos `quot` picosPerMinute
    in Concurrent.Time (picosecondsToDiffTime (toMinute * picosPerMinute))

doubleToDuration :: Double -> Concurrent.Duration
doubleToDuration = fromRational . toRational

------------------------------------------------------------------------------
-- ## Simulation
------------------------------------------------------------------------------

runDeli
    :: StdGen
    -> Deli chanState ()
    -> DeliState
runDeli _gen (Deli conc) =
    --let !randomAction = Concurrent.runConcurrentT conc
    let !writerAction = Concurrent.runConcurrentT conc
        !res = execState writerAction freshState
    in res

runJob
    :: Job
    -> Deli chanState ()
runJob (Job start duration) = do
    Deli (Concurrent.sleep duration)
    nowTime <- Deli Concurrent.now
    let !sojourn = Concurrent.subtractTime nowTime start
        modifier s = s & numProcessed +~ 1
                       & sojournStatistics %~ TDigest.insert (realToFrac sojourn)
                       & perfectStatistics %~ TDigest.insert (realToFrac duration)
    Deli $ modify' modifier
    updateTemporalStats (FinishedJob nowTime sojourn)

updateTemporalStats
    :: FinishedJob
    -> Deli chanState ()
updateTemporalStats (FinishedJob endTime sojourn) = do
    let clampedEnd = clampMinutes endTime
    currentSlice <- Deli $ use currentMinute
    if currentSlice == clampedEnd
    then do
        let modifier s =
                s & currentDigest %~ TDigest.insert (realToFrac sojourn)
        Deli $ modify' modifier
    else do
        let modifier s =
                s & currentMinute .~ clampedEnd
                  & currentDigest .~ TDigest.singleton (realToFrac sojourn)
                  & temporalStats %~ Data.Map.Strict.insert currentSlice (digestToTimeSlice currentSlice (s ^. currentDigest))
        Deli $ modify' modifier


digestToTimeSlice
    :: Concurrent.Time
    -> TDigest compression
    -> TimesliceStats
digestToTimeSlice minute stats =
    TimesliceStats
        { _sliceStart = minute
        , _response50 = doubleToDuration (fromJust (quantile 0.5 stats))
        , _response99 = doubleToDuration (fromJust (quantile 0.99 stats))
        }

simulate
    :: StdGen
    -> [Job]
    -> (Concurrent.Channel Job -> Deli Job ())
    -> DeliState
simulate gen jobs process =
    runDeli gen $ do
        mainChan <- Deli (Concurrent.newChannel Nothing)
        let insertQueue = Concurrent.writeChannel mainChan
            scheduled = [(_jobStart job, insertQueue job) | job <- jobs]
        Deli (Concurrent.lazySchedule scheduled)
        process mainChan
