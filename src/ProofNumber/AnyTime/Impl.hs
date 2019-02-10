module ProofNumber.AnyTime.Impl
  ( AnyTimeT(..)
  )
where

import           DSpies.Prelude

import           Control.Monad.Logger          ( MonadLogger )
import           UnliftIO

import           ProofNumber.AnyTime.Class

newtype AnyTimeT m a = AnyTimeT {runAnyTimeT :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader r)

-- Copied instance for IdentityT
instance MonadUnliftIO m => MonadUnliftIO (AnyTimeT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO =
    AnyTimeT $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runAnyTimeT))
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    AnyTimeT $ withRunInIO $ \run -> inner (run . runAnyTimeT)

instance MonadUnliftIO m => MonadAnyTime (AnyTimeT m) where
  type Event (AnyTimeT m) = Async ()

  runUntilCancelled event act = do
    result <- newTVarIO (error "No value yet")
    let go :: m ()
        go = do
          (r, finished) <- runAnyTimeT act
          atomically $ writeTVar result r
          unless finished go
    AnyTimeT $ withAsync go $ \running -> waitEither_ event running
    readTVarIO result
