module Main where

import Control.Monad (replicateM_, forever)
import Data.Random.Source.PureMT (newPureMT)
import Deli (Channel, Deli, JobTiming(..))
import Deli.Printer (printResults)
import System.Random
import qualified Deli
import qualified Deli.Random

variableWorkers
    :: Int
    -> Int
    -> Channel JobTiming
    -> Deli JobTiming ()
variableWorkers outerNum innerNum queue =
     replicateM_ outerNum $
        Deli.fork $ do
            innerChan <- Deli.newChannel (Just innerNum)
            replicateM_ innerNum $
                Deli.fork $ forever $ do
                    job <- Deli.readChannel innerChan
                    Deli.runJob job
            forever $ do
                job <- Deli.readChannel queue
                Deli.writeChannel innerChan job

simulation
    :: IO ()
simulation = do
    gen <- newStdGen
    inputGen <- newPureMT
    let arrivals = Deli.Random.arrivalTimePoissonDistribution 100
        serviceTimes = Deli.Random.durationParetoDistribution 0.08
        jobs = take 1000000 $ Deli.Random.distributionToJobs arrivals serviceTimes inputGen
        res = Deli.simulate gen jobs (variableWorkers 10 10)
    printResults res

main
    :: IO ()
main = simulation
