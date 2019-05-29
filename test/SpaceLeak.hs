module Main where

import Control.Monad (replicateM_, forever)
import Data.Random.Source.PureMT (pureMT)
import Deli (Channel, Deli, JobTiming(..))
import Deli.Printer (printResults)
import System.Random
import qualified Deli
import qualified Deli.Random

innerWorker
    :: Channel JobTiming
    -> Deli JobTiming ()
innerWorker queue =
    forever $ Deli.readChannel queue >>= Deli.runJob

outerWorker
    :: Int
    -> Channel JobTiming
    -> Deli JobTiming ()
outerWorker innerNum queue = do
    innerChan <- Deli.newChannel (Just 1)
    replicateM_ innerNum $
        Deli.fork (innerWorker innerChan)
    forever $ do
        job <- Deli.readChannel queue
        Deli.writeChannel innerChan job

variableWorkers
    :: Int
    -> Int
    -> Channel JobTiming
    -> Deli JobTiming ()
variableWorkers outerNum innerNum queue =
     replicateM_ outerNum $
        Deli.fork $ (outerWorker innerNum queue)

simpleWorkers
    :: Int
    -> Channel JobTiming
    -> Deli JobTiming ()
simpleWorkers numWorkers queue =
    replicateM_ numWorkers $
        Deli.fork $ forever $ Deli.readChannel queue >>= Deli.runJob

simulation
    :: IO ()
simulation = do
    let gen = mkStdGen 30793702
        inputGen = pureMT 908147245
        arrivals = Deli.Random.arrivalTimePoissonDistribution 100
        serviceTimes = Deli.Random.durationParetoDistribution 0.08
        jobs = take 50000 $ Deli.Random.distributionToJobs arrivals serviceTimes inputGen
        res = Deli.simulate gen jobs (simpleWorkers 100)
    printResults res

main
    :: IO ()
main = simulation
