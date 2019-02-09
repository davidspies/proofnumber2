{-# LANGUAGE UndecidableInstances #-}

module ProofNumber.Solve.Impl
  ( Solver
  , runSolver
  )
where

import           DSpies.Prelude          hiding ( State )

import qualified Control.Monad.Reader          as Reader
import           Data.IORef
import qualified Data.Map                      as Map

import           ProofNumber.Game
import           ProofNumber.Solve.Class

newtype Solver g a = Solver (ReaderT (Env g) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env g))

data Env g = Env{ game :: g, posMap :: IORef (Map (State g) (Result g)) }

instance (IsGame g, Ord (State g)) => MonadSolveGame (Solver g) where
  type Game (Solver g) = g

  askGame = Reader.asks game
  lookupState s =
    fmap (Map.lookup s) . liftIO . readIORef =<< Reader.asks posMap
  writeState s r = do
    ref <- Reader.asks posMap
    liftIO $ modifyIORef ref (Map.insert s r)

runSolver :: g -> Solver g a -> IO a
runSolver game (Solver act) = do
  posMap <- newIORef Map.empty
  runReaderT act Env { .. }
