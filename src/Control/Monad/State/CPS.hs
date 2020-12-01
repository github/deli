{-# LANGUAGE Trustworthy, Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, BangPatterns,
      UndecidableInstances #-}
module Control.Monad.State.CPS (StateT(..)
    , runStateT
    , evalStateT
    , execStateT
    , mapStateT
    , State
    , runState
    , evalState
    , execState
    , module Control.Monad.State.Class) where
import Control.Monad.State.Class
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class

newtype StateT s m a = StateT { unStateT :: forall r. s -> (a -> s -> m r) -> m r }

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT m s = unStateT m s (\a s -> return (a, s))
{-# INLINABLE runStateT #-}

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = unStateT m s $ \a _ -> return a
{-# INLINABLE evalStateT #-}

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = unStateT m s $ \_ s -> return s
{-# INLINABLE execStateT #-}

mapStateT :: (Monad m, Monad n) => (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
-- This used to be implemented directly, but doing it this way produces identical
-- Core and is considerably simpler.
mapStateT t m = stateT $ \s -> t (runStateT m s)

instance Functor (StateT s m) where
    fmap f m = StateT $ \s c -> unStateT m s (c . f)
    {-# INLINABLE fmap #-}

instance Applicative (StateT s m) where
    pure x = StateT $ \s c -> c x s
    {-# INLINABLE pure #-}
    mf <*> ma = StateT $ \s c -> unStateT mf s $ \f s' -> unStateT ma s' (c . f)
    {-# INLINABLE (<*>) #-}
    m *> n = StateT $ \s c -> unStateT m s $ \_ s' -> unStateT n s' c
    {-# INLINABLE (*>) #-}

instance Monad (StateT s m) where
    return x = StateT $ \s c -> c x s
    m >>= k = StateT $ \s c -> unStateT m s $ \a s' -> unStateT (k a) s' c
    {-# INLINABLE (>>=) #-}
    (>>) = (*>)

instance MonadState s (StateT s m) where
    get = StateT $ \s c -> c s s
    {-# INLINABLE get #-}
    put s = StateT $ \_ c -> c () s
    {-# INLINABLE put #-}
    state f = StateT $ \s c -> uncurry c (f s)
    {-# INLINABLE state #-}

instance MonadTrans (StateT s) where
    lift m = StateT $ \s c -> m >>= \a -> c a s
    {-# INLINABLE lift #-}

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadReader e m => MonadReader e (StateT s m) where
  ask = lift ask

  local f m = stateT $ \s -> local f (runStateT m s)

instance MonadFix m => MonadFix (StateT s m) where
  mfix f = stateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s

instance MonadCont m => MonadCont (StateT s m) where
  callCC f = stateT $ \s -> callCC $ \c -> runStateT (f (\a -> stateT $ \s' -> c (a, s'))) s

-- A stricter version of 'state'. The latter uses the
-- lazy 'uncurry' function for some reason.
stateT :: Monad m => (s -> m (a, s)) -> StateT s m a
stateT f = StateT $ \s c -> do
  (a, s') <- f s
  c a s'
{-# INLINE stateT #-}

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState m s = runIdentity $ runStateT m s
{-# INLINE runState #-}

evalState :: State s a -> s -> a
evalState m s = runIdentity $ evalStateT m s
{-# INLINE evalState #-}

execState :: State s a -> s -> s
execState m s = runIdentity $ execStateT m s
{-# INLINE execState #-}
