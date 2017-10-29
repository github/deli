module Main where

import Control.Monad (forM_, replicateM_, when, forever, void)
import Control.Monad.Deli
import Control.Monad.Trans (liftIO)
import System.Random
import qualified Control.Monad.Concurrent as Concurrent

main :: IO ()
main = queueExample
--    concurrentExample
--    let n = 10 * 1000 * 1000
--    Concurrent.runConcurrentT (replicateM_ n (return ()))
--    evalContT (replicateM_ n (return ()))

concurrentExample :: IO ()
concurrentExample =
    Concurrent.runConcurrentT $ do
        chan <- Concurrent.newChannel (Just 1)
        replicateM_ 100 $
            Concurrent.fork $
                replicateM_ 10000 $ do
                    Concurrent.writeChannel chan True
                    Concurrent.sleep 1


        replicateM_ 10 $
            Concurrent.fork $
                replicateM_ (10 * 10000) $ do
                    _ <- Concurrent.readChannel chan
                    Concurrent.sleep 1

queueExample :: IO ()
queueExample = do
    gen <- newStdGen
    let jobs = [Job x 10 | x <- [0,10..(10 * 1000 * 1000) - 1]]
        action queue =
                    forever $ readChannel queue >>= runJob
        res = simulate gen jobs action
    print (length res)

examples :: IO ()
examples = do
    putStrLn "example:"
    example
    putStr "\n"

    putStrLn "example2:"
    example2
    putStr "\n"

    putStrLn "example3:"
    example3
    putStr "\n"

    putStrLn "example4:"
    example4
    putStr "\n"

    putStrLn "example5"
    example5
    putStr "\n"

    putStrLn "example6:"
    example6
    putStr "\n"

    putStrLn "example7:"
    example7
    putStr "\n"

printN
    :: Concurrent.Duration
    -> Concurrent.ConcurrentT chanState IO ()
printN time = do
    Concurrent.sleep time
    liftIO (print time)

example :: IO ()
example = Concurrent.runConcurrentT $ do
    Concurrent.fork $ printN 3
    Concurrent.fork $ printN 4
    replicateM_ 5 (printN 1)
    printN 10

example2 :: IO ()
example2 = Concurrent.runConcurrentT $ do
    liftIO (print 5)
    liftIO (print 5)
    Concurrent.sleep 2
    liftIO (print 4)
    liftIO (print 4)
    Concurrent.fork (liftIO (print 1))

example3 :: IO ()
example3 = Concurrent.runConcurrentT $ do
    chan <- Concurrent.newChannel (Just 1)
    liftIO (putStrLn "created a channel")
    Concurrent.fork $ do
        liftIO (putStrLn "in the forked process")
        Concurrent.writeChannel chan True
    liftIO (putStrLn "right before reading")
    val <- Concurrent.readChannel chan
    liftIO (print val)

example4 :: IO ()
example4 = Concurrent.runConcurrentT $ do
    chanA <- Concurrent.newChannel (Just 1)
    chanB <- Concurrent.newChannel (Just 1)
    Concurrent.fork $ do
        val <- Concurrent.readChannel chanA
        Concurrent.writeChannel chanB val

    Concurrent.writeChannel chanA True
    val <- Concurrent.readChannel chanB
    liftIO (print val)

example5 :: IO ()
example5 = Concurrent.runConcurrentT $ do
    Concurrent.now >>= liftIO . print
    Concurrent.sleep 2
    when True $ Concurrent.sleep 2
    Concurrent.now >>= liftIO . print

example6 :: IO ()
example6 = Concurrent.runConcurrentT $ do
    chanA <- Concurrent.newChannel (Just 1)
    Concurrent.writeChannel chanA (1 :: Int)
    Concurrent.writeChannel chanA (2 :: Int)

example7 :: IO ()
example7 = Concurrent.runConcurrentT $ do
    chanA <- Concurrent.newChannel Nothing
    Concurrent.writeChannel chanA (1 :: Int)
    Concurrent.writeChannel chanA (2 :: Int)

    Concurrent.readChannel chanA >>= liftIO . print
    Concurrent.readChannel chanA >>= liftIO . print
