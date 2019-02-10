module ProofNumber.Solve.Class where

import           DSpies.Prelude          hiding ( State )

import           Control.Monad.Logger

import           ProofNumber.Game
import           ProofNumber.Solve.Node

class IsGame (Game m) => ReadSolver m where
  type Game m
  askSelf :: m (Player (Game m))
  askGame :: m (Game m)
  askDesired :: m (Outcome (Game m))

class StoreState m where
  type Stored m
  lookupState :: Stored m -> m (Maybe Node)
  writeState :: Stored m -> Node -> m ()

type MonadSolveGame m
  = ( MonadLogger m
  , ReadSolver m
  , Eq (Player (Game m))
  , Show (State (Game m))
  , StoreState m
  , Stored m ~ State (Game m)
  )
