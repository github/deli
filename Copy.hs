{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Maybe
import Control.Monad (forever)
import Control.Monad.Trans.Cont (ContT, resetT, shiftT, runContT)
import Control.Monad.Cont (MonadCont, callCC)
import Control.Monad.State.Strict
import Data.PQueue.Min as PQueue
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Debug.Trace

newtype Time = Time DiffTime
    deriving (Show, Eq, Ord, Num, Enum)

data PriorityCoroutine m = PriorityCoroutine
    { _routine :: ContWrapper m (Maybe Integer)
    , _jumpId :: !Integer
    , _priority :: !Time
    }

instance Eq (PriorityCoroutine m)
    where (==) a b =  _priority a == _priority b

instance Ord (PriorityCoroutine m)
    where compare a b = compare (_priority a) (_priority b)

data ContState m = ContState
    { _nextId :: Integer
    , _routineList :: MinQueue (PriorityCoroutine m)
    , _currentlyExecuting :: Maybe Integer
    , _time :: Time
    }

newtype ContWrapper m a =
    ContWrapper
        { runContWrapper' :: ContT (Maybe Integer) (StateT (ContState m) m) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadCont, MonadState (ContState m))

runContWrapper
    :: Monad m
    => ContWrapper m (Maybe Integer)
    -> m (Maybe Integer)
runContWrapper routine =
    let defaultState = ContState 0 PQueue.empty Nothing 0
    in
    flip evalStateT defaultState $ runContT (resetT $ runContWrapper' routine) return


runNothing
    :: Monad m
    => ContWrapper m ()
    -> m (Maybe Integer)
runNothing routine =
    runContWrapper (routine *> return Nothing)

action
    :: MonadIO m
    => ContWrapper m ()
action =
    mapM_ sleepDirect [0,10..10000000]

sleep
    :: Monad m
    => Time
    -> ContWrapper m (Maybe Integer)
sleep time =
    withJump (addRoutine time) dequeue

sleepDirect
    :: Monad m
    => Time
    -> ContWrapper m (Maybe Integer)
sleepDirect time = do
    routineIdent <- callCC $ \k -> do
                        ident <- gets _nextId
                        addRoutine time (k (Just ident)) ident
                        let modifier s = s { _nextId = _nextId s + 1 }
                        modify modifier
                        return Nothing
    currentIdent <- gets _currentlyExecuting
    if routineIdent == currentIdent
    then
        return Nothing
    else
        ContWrapper $ shiftT $ \_ -> runContWrapper' dequeue

withJump
    :: Monad m
    => (ContWrapper m (Maybe Integer) -> Integer -> ContWrapper m ())
    -> ContWrapper m (Maybe Integer)
    -> ContWrapper m (Maybe Integer)
withJump callback whenBlocked = do
    routineIdent <- callCC $ \k -> do
                        ident <- gets _nextId
                        callback (k (Just ident)) ident
                        let modifier s = s { _nextId = _nextId s + 1 }
                        modify modifier
                        return Nothing
    currentIdent <- gets _currentlyExecuting
    if routineIdent == currentIdent
    then
        return Nothing
    else
        ContWrapper $ shiftT $ \_ -> runContWrapper' whenBlocked


addRoutine
    :: Monad m
    => Time
    -> ContWrapper m (Maybe Integer)
    -> Integer
    -> ContWrapper m ()
addRoutine time routine jumpId = do
    routines <- gets _routineList
    now <- gets _time
    let modifier s =
            s { _routineList = insertBehind (PriorityCoroutine routine jumpId (now + time)) routines
              }
    modify modifier

dequeue
    :: Monad m
    => ContWrapper m (Maybe Integer)
dequeue = do
    routines <- gets _routineList
    case PQueue.minView routines of
        Nothing ->
            return Nothing
        Just (PriorityCoroutine routine identifier priority, newQueue) -> do
            let modifier s =
                    s { _routineList = newQueue
                      , _currentlyExecuting = Just identifier
                      , _time = priority
                      }
            --traceM $ "now is: " ++ show priority
            modify modifier
            routine

main :: IO ()
main = do
    putStrLn "running"
    void (runContWrapper (action *> dequeue))
