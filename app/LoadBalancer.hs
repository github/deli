{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (replicateM, forM_, forever)
import Control.Monad.Loops (iterateM_)
import Control.Monad.Random.Class (getRandomR)
import Data.Coerce (coerce)
import Data.Random.Source.PureMT (newPureMT)
import Deli (Channel, Deli, JobTiming(..))
import Deli.Printer (printResults)
import System.Random
import qualified Data.PQueue.Min as PQueue
import qualified Deli
import qualified Deli.Random

createWorker
    :: Deli JobTiming (Channel JobTiming)
createWorker = do
    workerChannel <- Deli.newChannel Nothing
    Deli.fork $ forever $ do
        job <- Deli.readChannel workerChannel
        Deli.runJob job
    return workerChannel

roundRobinWorkers
    :: Int
    -> Channel JobTiming
    -> Deli JobTiming ()
roundRobinWorkers num jobChannel = do
    chans :: [Channel JobTiming] <- replicateM num createWorker
    -- create an infinite list of all channels, repeated,
    -- then for each one, read from main queue, and write
    -- to the worker's queue
    let roundRobinList = cycle chans
    forM_ roundRobinList $ \worker -> do
        job <- Deli.readChannel jobChannel
        Deli.writeChannel worker job

randomWorkers
    :: Int
    -> Channel JobTiming
    -> Deli JobTiming ()
randomWorkers num jobChannel = do
    chans :: [Channel JobTiming] <- replicateM num createWorker
    forever $ do
        randomWorkerIndex <- getRandomR (0, length chans - 1)
        let workerQueue = chans !! randomWorkerIndex
        job <- Deli.readChannel jobChannel
        Deli.writeChannel workerQueue job

data PriorityChannel = PriorityChannel
    { _pduration :: !Deli.Duration
    , _pchannel :: !(Deli.Channel JobTiming)
    } deriving (Eq, Ord, Show)

lwlDispatcher
    :: Deli.Channel JobTiming
    -> PQueue.MinQueue PriorityChannel
    -> Deli JobTiming ()
lwlDispatcher !readChan !queue = do
    now <- Deli.now
    iterateM_ (dispatch readChan) (queue, now)

dispatch
    :: Deli.Channel JobTiming
    -> (PQueue.MinQueue PriorityChannel, Deli.Time)
    -> Deli JobTiming (PQueue.MinQueue PriorityChannel, Deli.Time)
dispatch readChan !(queue, prevTime) = do
    job <- Deli.readChannel readChan
    newTime <- Deli.now

    durationMultiplier <- fromRational . toRational <$> getRandomR (0.7, 1.3 :: Float)


    let mFun lastTime nowTime (PriorityChannel d c) =
            PriorityChannel (max 0 (d - coerce (nowTime - lastTime))) c
        !adjustedQueue = PQueue.map (mFun prevTime newTime) queue
        (PriorityChannel shortestPrevDuration shortestQueue, deletedMin) = PQueue.deleteFindMin adjustedQueue

        approxJobDuration = durationMultiplier * _jobDuration job
        newPriorityChannel = PriorityChannel (shortestPrevDuration + approxJobDuration) shortestQueue
        !addedBack = PQueue.insert newPriorityChannel deletedMin

    Deli.writeChannel shortestQueue job
    return (addedBack, newTime)

leastWorkLeft
    :: Int
    -> Channel JobTiming
    -> Deli JobTiming ()
leastWorkLeft num jobChannel = do
    chans :: [Channel JobTiming] <- replicateM num createWorker
    let workQueue :: PQueue.MinQueue PriorityChannel
        startingTimes = take num [0.00001, 0.00002..]
        queueList = [PriorityChannel d c | (d, c)  <- zip startingTimes chans]
        workQueue = PQueue.fromAscList queueList
    lwlDispatcher jobChannel workQueue

loadBalancerExample :: IO ()
loadBalancerExample = do
    simulationGen <- newStdGen
    inputGen <- newPureMT
    let arrivals = Deli.Random.arrivalTimePoissonDistribution 1500
        serviceTimes = Deli.Random.durationExponentialDistribution 0.025
        numTests = 1000 * 1000 * 2
        jobsA = take numTests $ Deli.Random.distributionToJobs arrivals serviceTimes inputGen
        jobsB = take numTests $ Deli.Random.distributionToJobs arrivals serviceTimes inputGen
        jobsC = take numTests $ Deli.Random.distributionToJobs arrivals serviceTimes inputGen
        roundRobinRes = Deli.simulate simulationGen jobsA (roundRobinWorkers 48)
        randomRes = Deli.simulate simulationGen jobsB (randomWorkers 48)
        leastWorkLeftRes = Deli.simulate simulationGen jobsC (leastWorkLeft 48)

    putStrLn "## Round Robin ##"
    printResults roundRobinRes
    putStrLn "## Random ##"
    printResults randomRes
    putStrLn "## LeastWorkLeft ##"
    printResults leastWorkLeftRes
    newline

    where newline = putStrLn "\n"

main :: IO ()
main = do
    loadBalancerExample
    newline

    where newline = putStrLn "\n"
