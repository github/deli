{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (forever)
import Control.Monad.Trans.Cont (ContT, resetT, shiftT, runContT)
import Control.Monad.Cont (MonadCont, callCC)
import Data.Functor.Identity
import Control.Monad.State.Strict

data ContState m = ContState
    { _nextId :: Integer
    , _routineList :: [(Integer, ContWrapper (Maybe Integer) m (Maybe Integer))]
    , _currentlyExecuting :: Maybe Integer
    }

newtype ContWrapper r m a =
    ContWrapper
        { runContWrapper' :: ContT r (StateT (ContState m) m) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadCont, MonadState (ContState m))

runContWrapper
    :: Monad m
    => ContWrapper r m r
    -> m r
runContWrapper routine =
    let defaultState = ContState 0 [] Nothing
    in
    flip evalStateT defaultState $ runContT (resetT $ runContWrapper' routine) return

action
    :: MonadIO m
    => ContWrapper (Maybe Integer) m (Maybe Integer)
action = do
    forever $ do
        routineIdent <- callCC $ \k -> do
                        ident <- gets _nextId
                        addRoutine (k (Just ident))
        currentIdent <- gets _currentlyExecuting
        if routineIdent == currentIdent
        then
            -- now we're running because `k' was called
            return Nothing
        else
            -- we've registered the continuation, now time
            -- to do whatever we were going to do next, the continuation
            -- hasn't actually been called yet
            ContWrapper $ shiftT $ \_ ->
                runContWrapper' dequeue
    return Nothing


addRoutine
    :: Monad m
    => ContWrapper (Maybe Integer) m (Maybe Integer)
    -> ContWrapper (Maybe Integer) m (Maybe Integer)
addRoutine routine = do
    routines <- gets _routineList
    identifier <- gets _nextId
    let modifier s =
            s { _routineList = routines ++ [(identifier, routine)]
              , _nextId = identifier + 1
              }
    modify modifier
    return (Just identifier)

dequeue
    :: Monad m
    => ContWrapper (Maybe Integer) m (Maybe Integer)
dequeue = do
    routines <- gets _routineList
    case routines of
        [] ->
            return Nothing
        ((identifier, routine):tl) -> do
            let modifier s =
                    s { _routineList = tl
                      , _currentlyExecuting = Just identifier
                      }
            modify modifier
            routine

main :: IO ()
main = do
    putStrLn "running"
    void (runContWrapper action)
