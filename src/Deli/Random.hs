{-# LANGUAGE BangPatterns #-}

module Deli.Random
    ( distributionToJobs
    , distributionToList
    , arrivalTimePoissonDistribution
    , durationExponentialDistribution
    , durationParetoDistribution
    ) where

import Data.Random (RVar, sampleState)
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Distribution.Pareto (pareto)
import Data.Random.Source.PureMT (PureMT)
import Data.Time.Clock (diffTimeToPicoseconds)
import Deli (Time, Duration(..), JobTiming(..), microsecondsToDuration, microsecondsToTime)

distributionToJobs
    :: RVar Time
    -> RVar Duration
    -> PureMT
    -> [JobTiming]
distributionToJobs timing duration gen =
    let jobTimingR = JobTiming <$> timing <*> duration
        jobs = distributionToList jobTimingR gen
        addTimings (JobTiming a _) (JobTiming b d) = JobTiming (a + b) d
    in scanl1 addTimings jobs

distributionToList
    :: RVar a
    -> PureMT
    -> [a]
distributionToList dist gen =
    let (!val, newGen) = sampleState dist gen
    in (val : distributionToList dist newGen )

arrivalTimePoissonDistribution
    :: Double -- ^ Mean number of arrivals per second
    -> RVar Time
arrivalTimePoissonDistribution rate =
    let inverseRate = 1 / rate
        expDist = exponential inverseRate
        doubleToTime d = round (d * 1000 * 1000)
    in microsecondsToTime . doubleToTime <$> expDist

durationExponentialDistribution
    :: Duration -- ^ Mean service time
    -> RVar Duration
durationExponentialDistribution (Duration diffTime) =
    let picosDuration = diffTimeToPicoseconds diffTime
        oneSecondInPicos = 1000 * 1000 * 1000 :: Double
        expDist = exponential (fromIntegral picosDuration / oneSecondInPicos)
        doubleToDuration d = round (d * 1000)
    in microsecondsToDuration . doubleToDuration <$> expDist

-- |Create a Duration Pareto distribution from a mean service time. Note that
-- this hardcodes an alpha (α) of 1.16 (log4 5), which is used for the 80-20
-- "Pareto priciple" distribution.
durationParetoDistribution
    :: Duration -- ^ Mean service time
    -> RVar Duration
durationParetoDistribution (Duration diffTime) =
    let picosDuration = diffTimeToPicoseconds diffTime
        picosMeanToScale = fromIntegral picosDuration * (0.1379 :: Double)
        oneSecondInPicos = 1000 * 1000 * 1000 :: Double
        paretoDist = pareto (picosMeanToScale / oneSecondInPicos) 1.16
        doubleToDuration d = round (d * 1000)
    in microsecondsToDuration . doubleToDuration <$> paretoDist
