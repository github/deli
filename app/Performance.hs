import qualified Control.Monad.Concurrent as C
import Control.Monad (forever, replicateM_)
import Deli (Channel, Deli, JobTiming(..))
import Deli.Printer (printResults)
import System.Random
import qualified Deli

singleQueue
    :: Channel JobTiming
    -> Deli JobTiming ()
singleQueue queue =
    forever $ do
        job <- Deli.readChannel queue
        Deli.runJob job

singleQueueExample :: IO ()
singleQueueExample = do
    gen <- newStdGen
    let durations = repeat 0.5
        count = 1000 * 100
        times = [0,1..(count - 1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs singleQueue
    printResults res

chainedQueues
    :: Channel JobTiming
    -> Deli JobTiming ()
chainedQueues queue = do
    middleChan <- Deli.newChannel Nothing
    Deli.fork $ forever $ do
        job <- Deli.readChannel middleChan
        Deli.runJob job
    forever $ do
        job <- Deli.readChannel queue
        Deli.writeChannel middleChan job

chainedQueueExample :: IO ()
chainedQueueExample = do
    gen <- newStdGen
    let durations = repeat 0.5
        count = 1000 * 100
        times = [0,1..(count - 1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs chainedQueues
    printResults res

oneThread
    :: Channel JobTiming
    -> Deli JobTiming ()
oneThread queue = do
    middleChan <- Deli.newChannel (Just 1)
    forever $ do
        jobA <- Deli.readChannel queue
        Deli.writeChannel middleChan jobA
        jobB <- Deli.readChannel middleChan
        Deli.runJob jobB

oneThreadExample :: IO ()
oneThreadExample = do
    gen <- newStdGen
    let durations = repeat 0.5
        count = 1000 * 1000
        times = [0,1..(count - 1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs oneThread
    printResults res

concurrentSingleExample
    :: IO ()
concurrentSingleExample =
    C.runConcurrentT $ do
        chan <- C.newChannel (Just 1)
        C.fork $ forever $
            C.readChannel chan >> return ()
        replicateM_ (1000 * 100 * 10) $ do
            C.writeChannel chan True

concurrentChainedExample
    :: IO ()
concurrentChainedExample =
    C.runConcurrentT $ do
        chanOne <- C.newChannel (Just 1)
        chanTwo <- C.newChannel (Just 1)
        C.fork $ forever $ do
            val <- C.readChannel chanOne
            C.writeChannel chanTwo val
        C.fork $ forever $
            C.readChannel chanTwo >> return ()
        replicateM_ (1000 * 100 * 10) $ do
            C.writeChannel chanOne True

main :: IO ()
main = do
    newline
    putStrLn "## singleQueueExample ##"
    singleQueueExample
    newline

    newline
    putStrLn "## chainedQueueExample ##"
    chainedQueueExample
    newline

    newline
    putStrLn "## oneThreadExample ##"
    oneThreadExample
    newline

    concurrentSingleExample
    concurrentChainedExample

    where newline = putStrLn "\n"
