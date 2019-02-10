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

data Costs = Costs {selfCost :: Integer, othersCost :: Integer}

data Perspective = Self | Other

opposite :: Perspective -> Perspective
opposite = \case
  Self  -> Other
  Other -> Self

makeNode :: Perspective -> Infinible Integer -> Infinible Integer -> Node
makeNode Self  Infinity            _                   = Finished False
makeNode Self  _                   Infinity            = Finished True
makeNode Other Infinity            _                   = Finished True
makeNode Other _                   Infinity            = Finished False
makeNode Self  (Finite selfCost  ) (Finite othersCost) = Remaining Costs { .. }
makeNode Other (Finite othersCost) (Finite selfCost  ) = Remaining Costs { .. }

proofCost :: Perspective -> Node -> Infinible Integer
proofCost = \case
  Self -> \case
    Finished  False              -> Infinity
    Finished  True               -> Finite 0
    Remaining Costs { selfCost } -> Finite selfCost
  Other -> \case
    Finished  False                -> Finite 0
    Finished  True                 -> Infinity
    Remaining Costs { othersCost } -> Finite othersCost

combineResults :: Perspective -> [Node] -> Node
combineResults p ns = makeNode p pCost oppCost
 where
  pCost   = minimum $ map (proofCost p) ns
  oppCost = sum $ map (proofCost (opposite p)) ns
