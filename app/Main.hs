{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forM_, replicateM_, replicateM, when, forever, unless, void)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Trans (liftIO)
import Control.Parallel
import Data.List (foldl', insertBy, sortOn, scanl')
import Data.Maybe (fromJust, catMaybes)
import Data.Random
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Source.PureMT
import Data.TDigest
import Data.Time (picosecondsToDiffTime)
import Deli
import System.Random
import qualified Control.Monad.Concurrent as Concurrent
import qualified Data.Map.Strict as Map

main :: IO ()
main = simpleQueueExample

threadIdExample :: IO ()
threadIdExample = Concurrent.runConcurrentT $ do
    chan <- Concurrent.newChannel (Just 1)
    mainId <- Concurrent.threadId
    liftIO $ putStrLn $ "main Id is " ++ show mainId
    Concurrent.fork $ do
        secondId <- Concurrent.threadId
        liftIO $ putStrLn $ "second Id is " ++ show secondId
        Concurrent.sleep 5
        secondId' <- Concurrent.threadId
        liftIO $ putStrLn $ "second Id after sleep is " ++ show secondId'
        _ <- Concurrent.readChannel chan
        secondId'' <- Concurrent.threadId
        liftIO $ putStrLn $ "second Id after channel read is " ++ show secondId''
    Concurrent.fork $ do
        thirdId <- Concurrent.threadId
        liftIO $ putStrLn $ "third Id is " ++ show thirdId
        Concurrent.sleep 1
        thirdId' <- Concurrent.threadId
        liftIO $ putStrLn $ "third Id after sleep is " ++ show thirdId'
        _ <- Concurrent.readChannel chan
        thirdId'' <- Concurrent.threadId
        liftIO $ putStrLn $ "third Id after channel read is " ++ show thirdId''

    Concurrent.sleep 6
    mainId' <- Concurrent.threadId
    liftIO $ putStrLn $ "main Id after sleep is " ++ show mainId'

    Concurrent.writeChannel chan ()
    Concurrent.writeChannel chan ()

    mainId'' <- Concurrent.threadId
    liftIO $ putStrLn $ "main Id after channel writes is " ++ show mainId''


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
        times = [0,1..(100000-1)]
        jobs = zipWith JobTiming times durations
        res = simulate gen jobs (simpleAction 1)
    printResults res

webhooks queue = do
    slowQueue <- newChannel Nothing
    fastQueue <- newChannel Nothing
    replicateM_ 80 $
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
    else return 30

webhooksExample :: IO ()
webhooksExample = do
    gen <- newStdGen
    mtGen <- newPureMT
    let durations = sampleDistribution webhooksDistribution mtGen
        randomTimeDurations = max 0 <$> sampleDistribution (exponential 0.005) mtGen
        starts = scanl' addDuration 0 randomTimeDurations
    let jobs = takeWhile (\x -> _jobStart x < (60 * 60 * 24)) $ zipWith JobTiming starts durations
        resA = simulate gen jobs (simpleAction 140)
        resB = simulate gen jobs webhooks

    putStrLn "## Simple"
    printResults (par resB resA)
    putStrLn "\n"

    putStrLn "## Hi/Low"
    printResults resB
    putStrLn "\n"

printResults :: DeliState -> IO ()
printResults res = do
    putStrLn "Simulated wait (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (_waitStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (_waitStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (_waitStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (_waitStatistics res))
    putStrLn ""

    putStrLn "Simulated sojourn (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (_sojournStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (_sojournStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (_sojournStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (_sojournStatistics res))
    putStrLn ""

    putStrLn "Overall processing:"
    putStrLn $ "total number processed: " ++ show (_numProcessed res)

--    let values = [1000 * fromRational (toRational (_response50 v)) | (_, v) <- Map.toAscList (_temporalStats res)] :: [Double]
--    forM_ values $ \val -> do
--        putStr (show val)
--        putStr " "
--    putStrLn "\n"
