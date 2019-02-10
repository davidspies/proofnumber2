module ProofNumber.Solve.Node
  ( Costs(..)
  , Node(..)
  , Perspective(..)
  , combineResults
  , proofCost
  )
where

import           DSpies.Prelude

import           ProofNumber.Infinible

data Node = Finished Bool | Remaining Costs

data Costs = Costs
  { selfCost :: Double
  , othersCost :: Double
  , selfProb :: Double
  }

data Perspective = Self | Other

opposite :: Perspective -> Perspective
opposite = \case
  Self  -> Other
  Other -> Self

makeNode
  :: Perspective -> Infinible Double -> Infinible Double -> Double -> Node
makeNode Self  Infinity _        _ = Finished False
makeNode Self  _        Infinity _ = Finished True
makeNode Other Infinity _        _ = Finished True
makeNode Other _        Infinity _ = Finished False
makeNode Self (Finite selfCost) (Finite othersCost) selfProb =
  Remaining Costs { .. }
makeNode Other (Finite othersCost) (Finite selfCost) oppProb =
  Remaining Costs { selfProb = 1 - oppProb, .. }

proofCost :: Perspective -> Node -> Infinible Double
proofCost = \case
  Self -> \case
    Finished  False              -> Infinity
    Finished  True               -> Finite 0
    Remaining Costs { selfCost } -> Finite selfCost
  Other -> \case
    Finished  False                -> Finite 0
    Finished  True                 -> Infinity
    Remaining Costs { othersCost } -> Finite othersCost

proofProb :: Perspective -> Node -> Double
proofProb = \case
  Self  -> nodeSelfProb
  Other -> (1 -) . nodeSelfProb

nodeSelfProb :: Node -> Double
nodeSelfProb = \case
  Finished  False              -> 0
  Finished  True               -> 1
  Remaining Costs { selfProb } -> selfProb

combineResults :: Perspective -> [Node] -> Node
combineResults p ns = makeNode p pCost oppCost pProb
 where
  pCost   = minimum $ map (proofCost p) ns
  oppCost = sum $ map (proofCost (opposite p)) ns
  pProb   = maximum $ map (proofProb p) ns
