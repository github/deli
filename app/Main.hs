{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forM_, replicateM_, replicateM, when, forever, void)
import Data.List (sortOn, scanl')
import Control.Monad.Deli
import Control.Monad.Trans (liftIO)
import Data.TDigest
import System.Random
import qualified Control.Monad.Concurrent as Concurrent
import Data.Time (picosecondsToDiffTime)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map.Strict as Map

import Control.Parallel
import Data.Random
import Data.Random.Source.PureMT
import Data.Random.Distribution.Exponential (exponential)

main :: IO ()
main = webhooksExample

concurrentExample :: IO ()
concurrentExample =
    Concurrent.runConcurrentT $ do
        chan <- Concurrent.newChannel (Just 1)
        replicateM_ 100 $
            Concurrent.fork $
                replicateM_ 10000 $ do
                    Concurrent.writeChannel chan True
                    Concurrent.sleep 1


        replicateM_ 10 $
            Concurrent.fork $
                replicateM_ (10 * 10000) $ do
                    _ <- Concurrent.readChannel chan
                    Concurrent.sleep 1

sampleDistribution
    :: RVar Double
    -> PureMT
    -> [Duration]
sampleDistribution dist gen =
    let (!val, newGen) = sampleState dist gen
    in (doubleToDuration val : sampleDistribution dist newGen )

doubleToDuration :: Double -> Duration
doubleToDuration x =
    millisecondsToDuration (round (x * 1000))

uQuantile
    :: Double
    -> TDigest comp
    -> Double
uQuantile q digest =
    1000 * fromJust (quantile q digest)

simpleAction num queue =
     replicateM_ num $
        fork $ forever $ do
            job <- readChannel queue
            runJob job

simpleQueueExample :: IO ()
simpleQueueExample = do
    gen <- newStdGen
    let durations = cycle [0.8, 0.9, 1.0, 1.1, 1.2]
        times = [0,1..(10000-1)]
        jobs = zipWith Job times durations
        res = simulate gen jobs (simpleAction 1)
    printResults res

webhooks queue = do
    slowQueue <- newChannel Nothing
    fastQueue <- newChannel Nothing
    replicateM_ 82 $
        fork $
            forever $
                readChannel fastQueue >>= runJob
    replicateM_ 60 $
        fork $
            forever $ do
                mSlowJob <- readChannelNonblocking slowQueue
                case mSlowJob of
                    Just job ->
                        runJob job
                    Nothing -> do
                        mFastJob <- readChannelNonblocking fastQueue
                        case mFastJob of
                            Nothing ->
                                readChannel slowQueue >>= runJob
                            Just fastJob ->
                                runJob fastJob

    forever $ do
        job <- readChannel queue
        if _jobDuration job < 20
        then writeChannel fastQueue job
        else writeChannel slowQueue job

webhooksDistribution :: RVar Double
webhooksDistribution = do
    n <- uniformT 0 (1 :: Double)
    if n < 0.9901
    then exponential 0.4
    else uniform 28 30

webhooksExample :: IO ()
webhooksExample = do
    gen <- newStdGen
    mtGen <- newPureMT
    let durations = sampleDistribution webhooksDistribution mtGen
        randomTimeDurations = max 0 <$> sampleDistribution (exponential 0.005) mtGen
        starts = scanl' addDuration 0 randomTimeDurations
    let jobs = takeWhile (\x -> _jobStart x < (60 * 60 * 48)) $ zipWith Job starts durations
        resA = simulate gen jobs (simpleAction 142)
        resB = simulate gen jobs webhooks
    putStrLn "## Naive Implementation"
    printResults (par resB resA)
    putStrLn ""

    putStrLn "## Hi/Low Implementation"
    printResults resB
    putStrLn ""

printResults :: DeliState -> IO ()
printResults res = do
    putStrLn "Simulated wait (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (_waitStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (_waitStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (_waitStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (_waitStatistics res))
    putStrLn ""

    putStrLn "Overall processing:"
    putStrLn $ "total number processed: " ++ show (_numProcessed res)

--    let values = [1000 * fromRational (toRational (_response50 v)) | (_, v) <- Map.toAscList (_temporalStats res)] :: [Double]
--    forM_ values $ \val -> do
--        putStr (show val)
--        putStr " "
--    putStrLn "\n"
