{-# LANGUAGE UndecidableInstances #-}

module ProofNumber.Solve.Impl
  ( Solver
  , runSolver
  )
where

import           DSpies.Prelude          hiding ( State )

import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST
import qualified Control.Monad.Trans           as Trans
import qualified Data.Map                      as Map
import           Data.STRef

import           ProofNumber.Game
import           ProofNumber.Solve.Class

newtype Solver g s a = Solver (ReaderT (Env g s) (ST s) a)
  deriving (Functor, Applicative, Monad, MonadReader (Env g s))

data Env g s = Env
  { game :: g
  , player :: Player g
  , objective :: Outcome g
  , posMap :: STRef s (Map (State g) Node)
  }

instance (IsGame g, Eq (Player g), Ord (State g))
    => MonadSolveGame (Solver g s) where
  type Game (Solver g s) = g

  askGame    = Reader.asks game
  askSelf    = Reader.asks player
  askDesired = Reader.asks objective
  lookupState s =
    fmap (Map.lookup s) . Solver . Trans.lift . readSTRef =<< Reader.asks posMap
  writeState s r = do
    ref <- Reader.asks posMap
    Solver . Trans.lift $ modifySTRef ref (Map.insert s r)

runSolver
  :: forall g a . g -> Player g -> Outcome g -> (forall s . Solver g s a) -> a
runSolver game player objective act0 = runST $ go act0
 where
  go :: forall s . Solver g s a -> ST s a
  go (Solver act) = do
    posMap <- newSTRef Map.empty
    runReaderT act Env { .. }
