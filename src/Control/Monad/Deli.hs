{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

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

import qualified Control.Monad.Concurrent as Concurrent
import Control.Monad.Random.Strict
import Control.Monad.Writer.Strict (MonadWriter, Writer, runWriter, tell)
import System.Random (StdGen)

data Job = Job
    { _jobStart :: Concurrent.Time
    , _jobDuration :: Concurrent.Duration
    } deriving (Show, Eq, Ord)

newtype FinishedJob = FinishedJob
    { _jobSojourn :: Concurrent.Duration
    } deriving (Show, Eq, Ord)

newtype Deli chanState a =
    Deli
        { getDeli :: Concurrent.ConcurrentT chanState () (RandT StdGen (Writer [FinishedJob])) a
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
    let randomAction = Concurrent.runConcurrentT conc
        writerAction = evalRandT randomAction gen
        (_, res) = runWriter writerAction
    in res

runJob
    :: Job
    -> Deli chanState ()
runJob (Job start duration) = do
    Deli (Concurrent.sleep duration)
    now <- Deli Concurrent.now
    let finished = FinishedJob (Concurrent.subtractTime now start)
    Deli (lift (tell [finished]))

simulate
    :: StdGen
    -> [Job]
    -> (Concurrent.Channel Job -> Deli Job ())
    -> [FinishedJob]
simulate gen jobs process =
    runDeli gen $ do
        mainChan <- Deli (Concurrent.newChannel Nothing)
        let insertQueue job = Deli (Concurrent.schedule (_jobStart job)
                                        (Concurrent.writeChannel mainChan job))
        mapM_ insertQueue jobs
        process mainChan

