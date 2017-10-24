{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (forever)
import Control.Monad.Trans.Cont
import Data.Functor.Identity
import Control.Monad.State.Strict

newtype ContWrapper r m a =
    ContWrapper
        { runContWrapper' :: ContT r (StateT (ContWrapper r m ()) m) a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadState (ContWrapper r m ()))

runContWrapper
    :: Monad m
    => ContWrapper r m r
    -> m r
runContWrapper routine =
    let defaultState = return ()
    in
    flip evalStateT defaultState $ runContT (resetT $ runContWrapper' routine) return

action
    :: Monad m
    => ContWrapper () m ()
action =
    forever $
        ContWrapper $ shiftT $ \k -> runContWrapper' $ do
               addRoutine (ContWrapper (lift (k ())))
               dequeue

addRoutine
    :: Monad m
    => ContWrapper () m ()
    -> ContWrapper () m ()
addRoutine = put

dequeue
    :: Monad m
    => ContWrapper () m ()
dequeue =
    join get

main :: IO ()
main = do
    putStrLn "running"
    runContWrapper action
