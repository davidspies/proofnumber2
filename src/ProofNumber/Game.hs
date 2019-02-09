module ProofNumber.Game where

import           DSpies.Prelude          hiding ( State )

class Ord (Outcome g) => IsGame g where
  type State g
  type Result g
  type Outcome g
  type Move g
  type Player g

  makeMove :: g -> Move g -> State g -> State g
  status :: g -> State g -> Maybe (Result g)
  outcome :: g -> Player g -> Result g -> Outcome g
  availableMoves :: g -> State g -> [Move g]
  turn :: g -> State g -> Player g
