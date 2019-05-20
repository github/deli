{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Control.Monad.Concurrent as C
import Control.Monad.State.Strict (State, execState, modify')
import Control.Monad (forever, forM_, replicateM_)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck (Arbitrary(..), Property, Positive(..), (===))

main
    :: IO ()
main = defaultMain concurrentTests

concurrentTests
    :: TestTree
concurrentTests =
    testGroup "Concurrent"
        [ concurrentUnitTests
        , concurrentQuickCheckTests
        ]

concurrentUnitTests
    :: TestTree
concurrentUnitTests =
    testGroup "Unit Tests"
        [ simpleWriterReader 0
        , simpleWriterReader 1
        , simpleWriterReader 10
        , simpleWriterReader 100
        , simpleWriterReader 1000

        , negativeDurationSleep
        ]

concurrentQuickCheckTests
    :: TestTree
concurrentQuickCheckTests =
    localOption (QuickCheckTests 100000) $
        testProperty "propWriterReader" propWriterReader

-- Set up an unbuffered channel, with a forked reader. In the main thread,
-- write `totalCount` values. The reader reads in an infinite loop. Using
-- a counter in the State monad, assert that `totalCount` messages made it
-- through the system.
simpleWriterReader
    :: Int
    -> TestTree
simpleWriterReader totalCount =
    let testName = "Simple writer/reader (" ++ show totalCount ++ ")"
    in
    testCase testName $ do
        let inc = modify' (+ 1)
            concurrentRes = C.runConcurrentT $ do
                c <- C.newChannel Nothing
                C.fork $
                    forever $ do
                        _ <- C.readChannel c
                        inc
                replicateM_ totalCount $
                    C.writeChannel c ()
            count = execState concurrentRes (0 :: Int)
        count @?= totalCount

negativeDurationSleep
    :: TestTree
negativeDurationSleep =
    testCase "Negative duration sleep" $ do
        let inc = modify' (+ 1)
            concurrentRes = C.runConcurrentT $ do
                C.sleep (-1)
                inc
            count = execState concurrentRes (0 :: Int)
        count @?= 1

-- This is an orphan instance, hence the pragma at the top to suppress warnings
instance Arbitrary C.Duration where
    arbitrary = fromRational <$> arbitrary

propWriterReader
    :: Positive Int
    -> [C.Duration]
    -> Property
propWriterReader (Positive numReaders) messages =
    let concurrentRes = C.runConcurrentT (concurrentAction numReaders messages)
        count = execState concurrentRes 0
    in
    count === length messages

concurrentAction
    :: Int
    -> [C.Duration]
    -> C.ConcurrentT C.Duration (State Int) ()
concurrentAction numReaders messages = do
    let inc = modify' (+ 1)
    -- TODO: randomly generate `Nothing` or `Just ...` buffer values
    chan <- C.newChannel Nothing
    replicateM_ numReaders $
        C.fork $ forever $ do
            val <- C.readChannel chan
            C.sleep val
            inc
    -- TODO: randomly generate the arrivals of the messages
    forM_ messages $ \m -> C.writeChannel chan m
