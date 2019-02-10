module ProofNumber.Solve.Class where

import           DSpies.Prelude          hiding ( State )

import           Control.Monad.Logger

import           ProofNumber.Game
import           ProofNumber.Solve.Node

class
    ( IsGame (Game m)
    , Eq (Player (Game m))
    , Monad m
    , MonadLogger m
    , Show (State (Game m))
    ) => MonadSolveGame m where
  type Game m
  askSelf :: m (Player (Game m))
  askGame :: m (Game m)
  askDesired :: m (Outcome (Game m))
  lookupState :: State (Game m) -> m (Maybe Node)
  writeState :: State (Game m) -> Node -> m ()
