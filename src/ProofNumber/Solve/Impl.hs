{-# LANGUAGE UndecidableInstances #-}

module ProofNumber.Solve.Impl
  ( Solver
  , runSolver
  )
where

import           DSpies.Prelude          hiding ( State )

import           Control.Monad.Logger
import qualified Control.Monad.Reader          as Reader
import           Data.IORef
import qualified Data.Map                      as Map

import           ProofNumber.Game
import           ProofNumber.Solve.Class
import           ProofNumber.Solve.Node

newtype Solver g a = Solver (ReaderT (Env g) (LoggingT IO) a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadLogger
           , MonadReader (Env g)
           )

data Env g = Env
  { game :: g
  , player :: Player g
  , objective :: Outcome g
  , posMap :: IORef (Map (State g) Node)
  }

instance IsGame g => ReadSolver (Solver g) where
  type Game (Solver g) = g
  askGame    = Reader.asks game
  askSelf    = Reader.asks player
  askDesired = Reader.asks objective

instance Ord (State g) => StoreState (Solver g) where
  type Stored (Solver g) = State g
  lookupState s =
    fmap (Map.lookup s) . liftIO . readIORef =<< Reader.asks posMap
  writeState s r = do
    ref <- Reader.asks posMap
    liftIO $ modifyIORef ref (Map.insert s r)

instance IsGame g => HasHeuristic (Solver g) where
  heuristic _ = return 0.5

runSolver
  :: forall g a . LogLevel -> g -> Player g -> Outcome g -> Solver g a -> IO a
runSolver minLogLevel game player objective (Solver act) = do
  posMap <- newIORef Map.empty
  runStderrLoggingT $ filterLogger (\_ ll -> ll >= minLogLevel) $ runReaderT
    act
    Env { .. }
