This file is generated, please edit [app/Tutorial.lhs](../app/Tutorial.lhs) instead.
***
Welcome to the Deli tutorial. Through a series of increasingly complex
examples, this tutorial will give you an idea of the power and usage for
Deli.

This example is also a literate Haskell file, which means this document
itself compiles and is executable. You can run it yourself and see the
output by running:

``` shell
$ stack build
$ stack run tutorial
```

First, let's begin with our imports:

``` haskell
module Main where

import Control.Monad (replicateM_, forever)
import Deli (Channel, Deli, JobTiming(..))
import qualified Deli
import Deli.Printer (printResults)
import System.Random
```

Simple Queues
-------------

Next, let's create our first example, of a single queue and worker. Work
will be placed on the main queue, and our worker will read from it, and
process each item in serial:

``` haskell
singleQueue
    :: Channel JobTiming
    -> Deli JobTiming ()
singleQueue queue =
    forever $ do
        job <- Deli.readChannel queue
        Deli.runJob job
```

As you can see, describing a very simple system like this has little
ceremony. Next, let's set up the rest of the simulation, and run it.

``` haskell
singleQueueExample :: IO ()
singleQueueExample = do
    gen <- newStdGen
    let durations = cycle [0.8, 0.9, 1.0, 1.1, 1.2]
        times = [0,1..(100000-1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs singleQueue
    printResults res
```

First we've created a new random number generator (the Deli type
implements `MonadRandom`, for convenient, reproducible random number
generation). Next, we create a dataset of our jobs, to be simulated. In
this case, jobs will take one of a set of durations (in seconds), with a
mean of `1.0`. Then we set it up so that jobs will be triggered from the
outside world once each second.

Finally, we run the simulation, passing in our random number seed, set
of jobs, and our implemented system (`singleQueueExample`).

Running the simulation, we get two primary sets of statistics. We see
the wait time (how long did our jobs have to wait in line before
processing begun), and their sojourn time, which is the wait time plus
the processing time.

In this case, we have a non-zero wait-time, which means we are sometimes
at capacity, and are queueing up work. This is also reflected in the
fact that the soujourn 50th percentile is greating (albeit slightly)
than one-second.

You will see output similar to this:

``` shell
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

``` haskell
variableWorkers
    :: Int
    -> Channel JobTiming
    -> Deli JobTiming ()
variableWorkers num queue =
     replicateM_ num $
        Deli.fork $ forever $ do
            job <- Deli.readChannel queue
            Deli.runJob job
```

Here we've simply parameterized the number of workers. For each worker,
we spawn a thread (using the Deli DSL), and enter an infinite loop to
read work from the shared queue. This expands our exposure to the Deli
API, as we've now seen `fork`, `readChannel`, `runJob`, and `simulate`.
Deli's core API exposes familiar programming concepts to create queues,
read and write to them, and fork (lightweight) threads. This allows you
to create a model of your system, using similar constructs to the actual
version. This is core to Deli.

``` haskell
twoWorkerQueueExample :: IO ()
twoWorkerQueueExample = do
    gen <- newStdGen
    let durations = cycle [0.8, 0.9, 1.0, 1.1, 1.2]
        times = [0,1..(100000-1)]
        jobs = zipWith JobTiming times durations
        res = Deli.simulate gen jobs (variableWorkers 2)
    printResults res
```

Now we can run our same example, and pass in two workers. Running this,
we see that the system never reaches capacity, as the wait time is
always zero. We won't be able to beat this performance.

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

A more complex example
----------------------

``` haskell
main :: IO ()
main = do
    putStrLn "## singleQueueExample ##"
    singleQueueExample
    newline

    putStrLn "## twoWorkerQueueExample ##"
    twoWorkerQueueExample
    newline

    where newline = putStrLn "\n"
```