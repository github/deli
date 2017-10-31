{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forM_, replicateM_, when, forever, void)
import Control.Monad.Deli
import Control.Monad.Trans (liftIO)
import Data.TDigest
import System.Random
import qualified Control.Monad.Concurrent as Concurrent
import Data.Time (picosecondsToDiffTime)
import Data.Maybe (fromJust)
import Data.Map.Strict

import Data.Random

main :: IO ()
main = queueExample

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

randomNormalDurations :: StdGen -> [Duration]
randomNormalDurations gen =
    let (!val, newGen) = sampleState (normal 0.5 0.4) gen
    in (doubleToDuration val : randomNormalDurations newGen )

doubleToDuration :: Double -> Duration
doubleToDuration x =
    millisecondsToDuration (round (x * 1000))

uQuantile
    :: Double
    -> TDigest comp
    -> Double
uQuantile q digest =
    fromJust (quantile q digest)

queueExample :: IO ()
queueExample = do
    gen <- newStdGen
    let durations = randomNormalDurations gen
        zero = 0
        five = 5 * Time (picosecondsToDiffTime (1000 * 1000000))
        starts = [zero,five..(60 * 60) - 1]
    let jobs = zipWith Job starts durations
        action queue =
            replicateM_ 103 $
                fork $
                    forever $ readChannel queue >>= runJob
        res = simulate gen jobs action
    putStrLn "Simulated:"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (_sojournStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (_sojournStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (_sojournStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (_sojournStatistics res))
    putStrLn ""

    putStrLn "Perfect:"

    putStrLn $ "perfect 99th: " ++ show (uQuantile 0.99 (_perfectStatistics res))
    putStrLn $ "perfect 50th: " ++ show (uQuantile 0.5 (_perfectStatistics res))
    putStrLn ""

    putStrLn "Overall processing:"
    putStrLn $ "total number processed: " ++ show (_numProcessed res)

    let values = [1000 * fromRational (toRational (_response50 v)) | (_, v) <- Data.Map.Strict.toAscList (_temporalStats res)] :: [Double]
    forM_ values $ \val -> do
        putStr (show val)
        putStr " "
    putStrLn "\n"
