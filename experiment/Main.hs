{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Trans.Cont (ContT, resetT, shiftT, runContT)
import Control.Monad.Cont (MonadCont)
import Control.Monad.State.Strict
import Data.PQueue.Min as PQueue
import Data.Time.Clock (DiffTime)

newtype Time = Time DiffTime
    deriving (Show, Eq, Ord, Num, Enum)

data PriorityCoroutine m = PriorityCoroutine
    { _routine :: ContWrapper m ()
    , _priority :: !Time
    }

instance Eq (PriorityCoroutine m)
    where (==) a b =  _priority a == _priority b

instance Ord (PriorityCoroutine m)
    where compare a b = compare (_priority a) (_priority b)

data ContState m = ContState
    { _routineList :: MinQueue (PriorityCoroutine m)
    , _time :: Time
    }

newtype ContWrapper m a =
    ContWrapper
        { runContWrapper' :: ContT () (StateT (ContState m) m) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadCont, MonadState (ContState m))

runContWrapper
    :: MonadIO m
    => ContWrapper m ()
    -> m ()
runContWrapper routine =
    let defaultState = ContState PQueue.empty 0
        resetAction = do
            resetT $ runContWrapper' routine
            runContWrapper' dequeue
    in
    flip evalStateT defaultState $ runContT resetAction return


action
    :: MonadIO m
    => ContWrapper m ()
action = do
    mapM_ sleep [0,10..10000000]
    now <- gets _time
    liftIO $ print now

sleep
    :: MonadIO m
    => Time
    -> ContWrapper m ()
sleep time = do
    liftIO $ putStrLn "sleep starting"
    ContWrapper $ shiftT $ \k -> runContWrapper' $
        addRoutine time (ContWrapper (lift (k ())))
    liftIO $ putStrLn "sleep ending"

addRoutine
    :: MonadIO m
    => Time
    -> ContWrapper m ()
    -> ContWrapper m ()
addRoutine time routine = do
    routines <- gets _routineList
    now <- gets _time
    let modifier s =
            s { _routineList = insertBehind (PriorityCoroutine routine (now + time)) routines
              }
    modify modifier

dequeue
    :: MonadIO m
    => ContWrapper m ()
dequeue = do
    routines <- gets _routineList
    case PQueue.minView routines of
        Nothing -> do
            liftIO $ putStrLn "dequeue didn't find anything"
            return ()
        Just (PriorityCoroutine routine priority, newQueue) -> do
            liftIO $ putStrLn "dequeue did find something"
            let modifier s =
                    s { _routineList = newQueue
                      , _time = priority
                      }
            modify modifier
            routine
            --ContWrapper (resetT (runContWrapper' routine))
            dequeue

main :: IO ()
main = do
    putStrLn "running"
    void (runContWrapper action)
