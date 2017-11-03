module Main where

import Control.Lens ((^.))
import Control.Monad (forever, replicateM_)
import Control.Monad.Loops (unfoldM')
import Control.Parallel (par)
import Data.Sequence
import Data.Maybe (fromJust)
import Data.TDigest (TDigest, quantile)
import Deli.Webhooks (WebhookDelivery(..), readWebhookDeliveries)
import System.Random (newStdGen)
import qualified Deli

main :: IO ()
main = do
    gen <- newStdGen
    deliveries <- readWebhookDeliveries "truncated-sorted-log.csv"

    let resA = Deli.simulate gen deliveries production
        resB = Deli.simulate gen deliveries hrrn

    putStrLn "## Production"
    printResults (par resB resA)
    putStrLn "\n"

    putStrLn "## Highest Response-Ratio Next"
    printResults resB
    putStrLn "\n"

production
    :: Deli.Channel WebhookDelivery
    -> Deli.Deli WebhookDelivery ()
production queue =
    replicateM_ 448 $
        Deli.fork $
            forever $ Deli.readChannel queue >>= Deli.runJob

highLow
    :: Deli.Channel WebhookDelivery
    -> Deli.Deli WebhookDelivery ()
highLow queue = do
    slowQueue <- Deli.newChannel Nothing
    fastQueue <- Deli.newChannel Nothing
    replicateM_ 95 $
        Deli.fork $
            forever $
                Deli.readChannel fastQueue >>= Deli.runJob
    replicateM_ 5 $
        Deli.fork $
            forever $ do
                mSlowJob <- Deli.readChannelNonblocking slowQueue
                case mSlowJob of
                    Just job ->
                        Deli.runJob job
                    Nothing -> do
                        mFastJob <- Deli.readChannelNonblocking fastQueue
                        case mFastJob of
                            Nothing ->
                                Deli.readChannel slowQueue >>= Deli.runJob
                            Just fastJob ->
                                Deli.runJob fastJob

    forever $ do
        job <- Deli.readChannel queue
        if Deli._jobDuration (job ^. Deli.jobTiming) < 1
        then Deli.writeChannel fastQueue job
        else Deli.writeChannel slowQueue job

-- writes from the _right_ side of the sequence
writeM
    :: Deli.Channel a
    -> Seq a
    -> Deli.Deli a (Seq a)
writeM chan s =
    case viewr s of
        EmptyR -> return s
        ss :> h -> do
            Deli.writeChannel chan h

            loop chan ss
            where loop q x =
                    case viewr x of
                    EmptyR -> return Data.Sequence.empty
                    (xx :> xh) -> do
                        res <- Deli.writeChannelNonblocking chan xh
                        case res of
                            Nothing -> return x
                            Just _ -> loop q xx

hrrn
    :: Deli.Channel WebhookDelivery
    -> Deli.Deli WebhookDelivery ()
hrrn queue = do
    sortedChan <- Deli.newChannel (Just 1)
    replicateM_ 448 $
        Deli.fork $
            forever $
                Deli.readChannel sortedChan >>= Deli.runJob

    loop sortedChan Data.Sequence.empty

    where loop chan acc =
            if Data.Sequence.null acc
            then do
                blockingJob <- Deli.readChannel queue
                loop chan (Data.Sequence.singleton blockingJob)
            else do
                jobs <- unfoldM' (Deli.readChannelNonblocking queue)
                time <- Deli.now
                let prioritySeq = acc >< jobs
                    sFun a b = compare (Deli.priority time a) (Deli.priority time b)
                    rSorted = unstableSortBy sFun prioritySeq
                if Data.Sequence.null rSorted
                then loop chan Data.Sequence.empty
                else do
                    unwritten <- writeM chan rSorted
                    loop chan unwritten


uQuantile
    :: Double
    -> TDigest comp
    -> Double
uQuantile q digest =
    1000 * fromJust (quantile q digest)

printResults :: Deli.DeliState -> IO ()
printResults res = do
    putStrLn "Simulated wait (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (Deli._waitStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (Deli._waitStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (Deli._waitStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (Deli._waitStatistics res))
    putStrLn ""

    putStrLn "Simulated sojourn (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (Deli._sojournStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (Deli._sojournStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (Deli._sojournStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (Deli._sojournStatistics res))
    putStrLn ""

    putStrLn "Real job distribution (milliseconds):"

    putStrLn $ "simulated 99th: " ++ show (uQuantile 0.99 (Deli._perfectStatistics res))
    putStrLn $ "simulated 95th: " ++ show (uQuantile 0.95 (Deli._perfectStatistics res))
    putStrLn $ "simulated 75th: " ++ show (uQuantile 0.75 (Deli._perfectStatistics res))
    putStrLn $ "simulated 50th: " ++ show (uQuantile 0.50 (Deli._perfectStatistics res))
    putStrLn ""

    putStrLn "Overall processing:"
    putStrLn $ "total number processed: " ++ show (Deli._numProcessed res)
