{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ProofNumber.AnyTime.Class
  ( IsFinished
  , MonadAnyTime(..)
  , runUntilCancelledAndFinalize
  )
where

import           DSpies.Prelude

import           Control.Monad.Logger
import           Control.Monad.Trans.Control

type IsFinished = Bool

class Monad m => MonadAnyTime m where
  type Event m
  runUntilCancelled :: Event m -> m (a, IsFinished) -> m a

class MonadTransControl t => MonadHasFinished t where
  extractFinished :: StT t (a, IsFinished) -> (StT t a, IsFinished)

instance (MonadHasFinished t, MonadAnyTime m, Monad (t m))
    => MonadAnyTime (Transformed t m) where
  type Event (Transformed t m) = Event m
  runUntilCancelled e (Transformed (act :: t m (a, IsFinished))) =
    Transformed $ do
      x <- liftWith
        (\run -> runUntilCancelled e (extractFinished @t @a <$> run act))
      restoreT (return x)

instance MonadHasFinished (ReaderT r) where
  extractFinished = id
instance MonadHasFinished (ExceptT e) where
  extractFinished = \case
    Left  err    -> (Left err, True)
    Right (v, b) -> (Right v, b)
instance Monoid w => MonadHasFinished (WriterT w) where
  extractFinished ((v, b), w) = ((v, w), b)
instance MonadHasFinished (StateT s) where
  extractFinished ((v, b), s) = ((v, s), b)
instance MonadHasFinished LoggingT where
  extractFinished = id
instance MonadHasFinished NoLoggingT where
  extractFinished = id

deriving via Transformed (ReaderT r) m instance MonadAnyTime m
  => MonadAnyTime (ReaderT r m)
deriving via Transformed (ExceptT e) m instance MonadAnyTime m
  => MonadAnyTime (ExceptT e m)
deriving via Transformed (WriterT w) m instance (MonadAnyTime m, Monoid w)
  => MonadAnyTime (WriterT w m)
deriving via Transformed (StateT s) m instance (MonadAnyTime m)
  => MonadAnyTime (StateT s m)
deriving via Transformed LoggingT m instance (MonadAnyTime m)
  => MonadAnyTime (LoggingT m)
deriving via Transformed NoLoggingT m instance (MonadAnyTime m)
  => MonadAnyTime (NoLoggingT m)

runUntilCancelledAndFinalize
  :: MonadAnyTime m => Event m -> m (m a, IsFinished) -> m a
runUntilCancelledAndFinalize = join .: runUntilCancelled
