Welcome to the Deli tutorial. Through a series of increasingly complex
examples, this tutorial will give you an idea of the power and usage for Deli.

This example is also a literate Haskell file, which means this document itself
compiles and is executable. You can run it yourself and see the output by
running:

```shell
$ stack build
$ stack run tutorial
```

First, let's begin with our imports:

\begin{code}
module Main where

import Control.Monad (replicateM_, forever)
import Data.Random.Source.PureMT (newPureMT)
import Data.Typeable (Typeable)
import Deli (Channel, Deli, JobTiming(..))
import Deli.Printer (printResults)
import System.Random
import qualified Deli
import qualified Deli.Random
\end{code}

Simple Queues
---

Next, let's create our first example, of a single queue and worker. Work will
be placed on the main queue, and our worker will read from it, and process each
item in serial:

\begin{code}
singleQueue
    :: Channel JobTiming
    -> Deli ()
singleQueue queue =
    forever $ do
        job <- Deli.readChannel queue
        Deli.runJob job
\end{code}

As you can see, describing a very simple system like this has little ceremony.
Next, let's set up the rest of the simulation, and run it.

\begin{code}
singleQueueExample :: IO ()
singleQueueExample = do
    gen <- newStdGen
    let durations = cycle [0.8, 0.9, 1.0, 1.1, 1.2]
        times = [0,1..(100000-1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs singleQueue
    printResults res
\end{code}

First we've created a new random number generator (the Deli type implements
`MonadRandom`, for convenient, reproducible random number generation). Next, we
create a dataset of our jobs, to be simulated. In this case, jobs will take one
of a set of durations (in seconds), with a mean of `1.0`. Then we set it up so
that jobs will be triggered from the outside world once each second.

Finally, we run the simulation, passing in our random number seed, set of jobs,
and our implemented system (`singleQueueExample`).

Running the simulation, we get two primary sets of statistics. We see the wait
time (how long did our jobs have to wait in line before processing begun), and
their sojourn time, which is the wait time plus the processing time.

In this case, we have a non-zero wait-time, which means we are sometimes at
capacity, and are queueing up work. This is also reflected in the fact that the
soujourn 50th percentile is greating (albeit slightly) than one-second.

You will see output similar to this:

```shell
Simulated wait (milliseconds):
simulated 99th: 294.99974998749934
simulated 95th: 274.9987499374968
simulated 75th: 181.24578114452865
simulated 50th: 87.4934373359334

Simulated sojourn (milliseconds):
simulated 99th: 1295.0000000000002
simulated 95th: 1275.0000000000002
simulated 75th: 1181.2495312382812
simulated 50th: 1087.497187429686
```


Next, let's see what happens if we add more workers:

\begin{code}
variableWorkers
    :: (Deli.HasJobTiming jobType, Typeable jobType)
    => Int
    -> Channel jobType
    -> Deli ()
variableWorkers num queue =
     replicateM_ num $
        Deli.fork $ forever $ do
            job <- Deli.readChannel queue
            Deli.runJob job
\end{code}

Here we've simply parameterized the number of workers. For each worker, we
spawn a thread (using the Deli DSL), and enter an infinite loop to read work
from the shared queue. This expands our exposure to the Deli API, as we've now
seen `fork`, `readChannel`, `runJob`, and `simulate`. Deli's core API exposes
familiar programming concepts to create queues, read and write to them, and
fork (lightweight) threads. This allows you to create a model of your system,
using similar constructs to the actual version. This is core to Deli.

\begin{code}
twoWorkerQueueExample :: IO ()
twoWorkerQueueExample = do
    gen <- newStdGen
    let durations = cycle [0.8, 0.9, 1.0, 1.1, 1.2]
        times = [0,1..(100000-1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs (variableWorkers 2)
    printResults res
\end{code}

Now we can run our same example, and pass in two workers. Running this, we see
that the system never reaches capacity, as the wait time is always zero. We
won't be able to beat this performance.

```
Simulated wait (milliseconds):
simulated 99th: 0.0
simulated 95th: 0.0
simulated 75th: 0.0
simulated 50th: 0.0

Simulated sojourn (milliseconds):
simulated 99th: 1197.5
simulated 95th: 1187.5
simulated 75th: 1125.0
simulated 50th: 1000.0
```

A more complex example
---

Now, let's say we have an pareto distribution, with some requests
generally being quick, and others generally taking much longer. Let's compare
two implementations, one simply with twenty workers, and another with two separate
queues, partitioned by request type (using a total still of twenty workers).

Now let's create our two systems whose performance we want to compare.

\begin{code}
twentyWorkers
    :: Channel JobTiming
    -> Deli ()
twentyWorkers = variableWorkers 20

partitionedQueues
    :: Channel JobTiming
    -> Deli ()
partitionedQueues jobChannel = do
    -- We'll read work from the main queue, and then partition
    -- it into either the slow or fast queue.
    -- First, we create the two partitions, each with a buffer of 16.
    -- Instead, we could pass in Nothing for an unbounded queue.
    slowChannel <- Deli.newChannel (Just 16)
    fastChannel <- Deli.newChannel (Just 16)

    -- Each of our two workers will implement work stealing. The algorithm
    -- is as follows. First, check if your primary queue has work, if so,
    -- perform it. If not, check to see if the other queue has work, if so,
    -- per form it. If not, wait until your primary queue does have work.

    -- Spawn the slow workers
    replicateM_ 4 $
        Deli.fork $
            forever $ do
                mSlowJob <- Deli.readChannelNonblocking slowChannel
                case mSlowJob of
                    Just job ->
                        Deli.runJob job
                    Nothing -> do
                        mFastJob <- Deli.readChannelNonblocking fastChannel
                        case mFastJob of
                            Nothing ->
                                Deli.readChannel slowChannel >>= Deli.runJob
                            Just fastJob ->
                                Deli.runJob fastJob
    -- Spawn the fast workers
    replicateM_ 16 $
        Deli.fork $
            forever $ do
                mFastJob <- Deli.readChannelNonblocking fastChannel
                case mFastJob of
                    Just job ->
                        Deli.runJob job
                    Nothing -> do
                        mSlowJob <- Deli.readChannelNonblocking slowChannel
                        case mSlowJob of
                            Nothing ->
                                Deli.readChannel fastChannel >>= Deli.runJob
                            Just slowJob ->
                                Deli.runJob slowJob
    -- Loop forever, reading items, and putting them in the
    -- appropriate queue
    forever $ do
        item <- Deli.readChannel jobChannel
        -- If a job's duration is greater than 500 milliseconds,
        -- put it into the slow queue.

        -- In the real world, you'd likely have to predict the service
        -- time based on the parameters of the request, and in practice,
        -- that technique works remarkably well.
        if _jobDuration item > 0.5
        then Deli.writeChannel slowChannel item
        else Deli.writeChannel fastChannel item
\end{code}

We've set up our two implementations, now let's generate some example requests,
and compare results.

Instead of using a cycled list for our input data, we'll make things a bit more
realistic, and use a poisson process for arrival times, and a pareto
distribution for service times.

\begin{code}

paretoExample :: IO ()
paretoExample = do
    simulationGen <- newStdGen
    inputGen <- newPureMT
    -- Generate a poisson process of arrivals, with a mean of 650 arrivals
    -- per second
    let arrivals = Deli.Random.arrivalTimePoissonDistribution 650
    -- Generate a Pareto distribution of service times, with a mean service
    -- time of 3 milliseconds (0.03 seconds) (alpha is set to 1.16 inside this
    -- function)
        serviceTimes = Deli.Random.durationParetoDistribution 0.03
        jobs = take 200000 $ Deli.Random.distributionToJobs arrivals serviceTimes inputGen
        twentyWorkersRes = Deli.simulate simulationGen jobs twentyWorkers
        partitionedRes = Deli.simulate simulationGen jobs partitionedQueues

    putStrLn "## Pareto example  ##"
    putStrLn "## twentyWorkers ##"
    printResults twentyWorkersRes
    newline

    putStrLn "## partitionedQueues ##"
    printResults partitionedRes
    newline

    where newline = putStrLn "\n"
\end{code}

Interestingly enough, our more complex implementation is able to beat the
simple twenty workers. Intuitively, this is because with a Pareto distribution,
the occasional really slow job causes head of line blocking. By separating out
into a slow and fast queue (with work stealing), slow items will only block
other slow items, and when there are no slow items, all workers can be utilized
(via work stealing) to process fast jobs.

Note in particular how much better the work stealing algorithm does at the 95th
and 99th percentile of sojourn time.

\begin{code}
main :: IO ()
main = do
    putStrLn "## singleQueueExample ##"
    singleQueueExample
    newline

    putStrLn "## twoWorkerQueueExample ##"
    twoWorkerQueueExample
    newline

    paretoExample
    newline

    where newline = putStrLn "\n"
\end{code}


That's currently it for this tutorial, but we'll be looking to expand it in the
future.
