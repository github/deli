module Main where

import Control.Monad (forever, forM_, replicateM_)
import Data.Maybe (fromJust)
import Data.TDigest (TDigest, quantile)
import Deli.Webhooks (WebhookDelivery, readWebhookDeliveries)
import System.Random (newStdGen)
import qualified Deli

main :: IO ()
main = do
    gen <- newStdGen
    deliveries <- readWebhookDeliveries "sorted-log.csv"

    let res = Deli.simulate gen deliveries action

    printResults res

action
    :: Deli.Channel WebhookDelivery
    -> Deli.Deli WebhookDelivery ()
action queue =
    replicateM_ 120 $
        Deli.fork $
            forever $ Deli.readChannel queue >>= Deli.runJob

uQuantile
    :: Double
    -> TDigest comp
    -> Double
uQuantile q digest =
    1000 * fromJust (quantile q digest)

printResults :: Deli.DeliState -> IO ()
printResults res = do
    putStrLn "Wait time (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (Deli._sojournStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (Deli._sojournStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (Deli._sojournStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (Deli._sojournStatistics res))
    putStrLn ""

    putStrLn "Perfect:"

    putStrLn $ "perfect 99th: " ++ show (uQuantile 0.99 (Deli._perfectStatistics res))
    putStrLn $ "perfect 50th: " ++ show (uQuantile 0.5 (Deli._perfectStatistics res))
    putStrLn ""

    putStrLn "Overall processing:"
    putStrLn $ "total number processed: " ++ show (Deli._numProcessed res)
