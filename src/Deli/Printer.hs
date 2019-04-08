module Deli.Printer
    ( printResults
    ) where

import Data.Maybe (fromJust)
import Data.TDigest
import Deli
import Text.Printf (printf)



uQuantile
    :: Double
    -> TDigest comp
    -> Double
uQuantile q digest =
    1000 * fromJust (quantile q digest)

printTruncate :: String -> Double -> IO ()
printTruncate s d = do
    printf s d
    putStrLn ""

printResults :: DeliState -> IO ()
printResults res = do
    putStrLn "Simulated wait (milliseconds):"

    printTruncate "simulated 99th: %.2f" (uQuantile 0.99 (_waitStatistics res))
    printTruncate "simulated 95th: %.2f" (uQuantile 0.95 (_waitStatistics res))
    printTruncate "simulated 75th: %.2f" (uQuantile 0.75 (_waitStatistics res))
    printTruncate "simulated 50th: %.2f" (uQuantile 0.50 (_waitStatistics res))
    putStrLn ""

    putStrLn "Simulated sojourn (milliseconds):"

    printTruncate "simulated 99th: %.2f" (uQuantile 0.99 (_sojournStatistics res))
    printTruncate "simulated 95th: %.2f" (uQuantile 0.95 (_sojournStatistics res))
    printTruncate "simulated 75th: %.2f" (uQuantile 0.75 (_sojournStatistics res))
    printTruncate "simulated 50th: %.2f" (uQuantile 0.50 (_sojournStatistics res))
    putStrLn ""

    putStrLn "Overall processing:"
    putStrLn $ "total number processed: " ++ show (_numProcessed res)
