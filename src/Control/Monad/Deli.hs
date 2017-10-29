{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Deli
    ( Deli
    , Job(..)
    -- re-exported from Control.Monad.Concurrent
    , Concurrent.Time
    , Concurrent.Duration
    , Concurrent.Channel
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

import Control.Monad.Random.Strict
import Control.Monad.State.Strict (State, execState, modify')
import Data.Sequence ((|>))
import System.Random (StdGen)
import qualified Control.Monad.Concurrent as Concurrent
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Sequence

data Job = Job
    { _jobStart :: !Concurrent.Time
    , _jobDuration :: !Concurrent.Duration
    } deriving (Show, Eq, Ord)

newtype FinishedJob = FinishedJob
    { _jobSojourn :: Concurrent.Duration
    } deriving (Show, Eq, Ord)

newtype Deli chanState a =
    Deli
        { _getDeli :: Concurrent.ConcurrentT chanState (RandT StdGen (State (Sequence.Seq FinishedJob))) a
        } deriving (Functor, Applicative, Monad)

instance MonadRandom (Deli chanState) where
    getRandomR range = getRandomR range >>= Deli . pure

    getRandom = getRandom >>= Deli . pure

    getRandomRs range = getRandomRs range >>= Deli . pure

    getRandoms = getRandoms >>= Deli . pure

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
--
------------------------------------------------------------------------------

runDeli
    :: StdGen
    -> Deli chanState ()
    -> [FinishedJob]
runDeli gen (Deli conc) =
    let !randomAction = Concurrent.runConcurrentT conc
        !writerAction = evalRandT randomAction gen
        !res = execState writerAction Sequence.empty
    in Foldable.toList res

runJob
    :: Job
    -> Deli chanState ()
runJob (Job start duration) = do
    Deli (Concurrent.sleep duration)
    nowTime <- Deli Concurrent.now
    let !finished = FinishedJob (Concurrent.subtractTime nowTime start)
        fun s = s |> finished
    Deli (lift (modify' fun))

simulate
    :: StdGen
    -> [Job]
    -> (Concurrent.Channel Job -> Deli Job ())
    -> [FinishedJob]
simulate gen jobs process =
    runDeli gen $ do
        mainChan <- Deli (Concurrent.newChannel Nothing)
        let insertQueue = Concurrent.writeChannel mainChan
            scheduled = [(_jobStart job, insertQueue job) | job <- jobs]
        Deli (Concurrent.lazySchedule scheduled)
        process mainChan

