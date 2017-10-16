module Main where

import Control.Monad.Concurrent
import Control.Monad (replicateM_)
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
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

printN
    :: Duration
    -> ConcurrentT chanState () IO ()
printN time = do
    sleep time
    liftIO (print time)

example :: IO ()
example = runConcurrentT $ do
    fork $ replicateM_ 1 (printN 3)
    fork $ replicateM_ 1 (printN 4)
    replicateM_ 5 (printN 1)
    printN 10

example2 :: IO ()
example2 = runConcurrentT $ do
    liftIO (print 5)
    liftIO (print 5)
    sleep 2
    liftIO (print 4)
    liftIO (print 4)
    fork (liftIO (print 1))

example3 :: IO ()
example3 = runConcurrentT $ do
    chan <- newChannel 1
    liftIO (putStrLn "created a channel")
    fork $ do
        liftIO (putStrLn "in the forked process")
        writeChannel chan True
    liftIO (putStrLn "right before reading")
    val <- readChannel chan
    liftIO (print val)

example4 :: IO ()
example4 = runConcurrentT $ do
    chanA <- newChannel 1
    chanB <- newChannel 1
    fork $ do
        val <- readChannel chanA
        writeChannel chanB val

    writeChannel chanA True
    val <- readChannel chanB
    liftIO (print val)
