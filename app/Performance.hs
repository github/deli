import Control.Monad (forever)
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
    middleChan <- Deli.newChannel Nothing
    forever $ do
        jobA <- Deli.readChannel queue
        Deli.writeChannel middleChan jobA
        jobB <- Deli.readChannel middleChan
        Deli.runJob jobB

oneThreadExample :: IO ()
oneThreadExample = do
    gen <- newStdGen
    let durations = repeat 0.5
        count = 1000 * 100
        times = [0,1..(count - 1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs oneThread
    printResults res

main :: IO ()
main = do
    newline
    putStrLn "## singleQueueExample ##"
    newline
    singleQueueExample
    newline

    putStrLn "## chainedQueueExample ##"
    newline
    chainedQueueExample
    newline

    putStrLn "## oneThreadExample ##"
    newline
    oneThreadExample
    newline

    where newline = putStrLn "\n"
