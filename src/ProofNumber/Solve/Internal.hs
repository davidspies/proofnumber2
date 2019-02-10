{-# LANGUAGE TemplateHaskell #-}

module ProofNumber.Solve.Internal
  ( solve
  )
where

import           DSpies.Prelude          hiding ( State )

import           Control.Monad.Logger
import qualified Data.Text                     as Text

import           ProofNumber.Game
import           ProofNumber.Minimum
import           ProofNumber.Solve.Class
import           ProofNumber.Solve.Node

solve :: forall m . MonadSolveGame m => State (Game m) -> m Bool
solve s0 = do
  let go = lookupDefaultedState s0 >>= \case
        Finished b  -> return b
        Remaining{} -> do
          expand s0
          go
  go

lookupDefaultedState :: MonadSolveGame m => State (Game m) -> m Node
lookupDefaultedState s = do
  g <- askGame
  case status g s of
    Nothing -> lookupState s >>= \case
      Nothing -> do
        selfProb <- heuristic s
        return $ Remaining $ Costs { selfCost   = recip selfProb - 1
                                   , othersCost = recip (1 - selfProb) - 1
                                   , selfProb
                                   }
      Just r -> return r
    Just r -> do
      self    <- askSelf
      desired <- askDesired
      return $ Finished $ outcome g self r >= desired

expand :: MonadSolveGame m => State (Game m) -> m ()
expand s = lookupState s >>= \case
  Nothing -> do
    $logDebug $ Text.unwords ["Exploring", Text.pack $ show s]
    recompute s
  Just Finished{}  -> return ()
  Just Remaining{} -> do
    (myPerspective, descendants) <- getPerspectiveAndDescendants s
    chosen                       <- minimumOnA
      (fmap (proofCost myPerspective) . lookupDefaultedState)
      descendants
    expand chosen
    recompute s

getPerspectiveAndDescendants
  :: MonadSolveGame m => State (Game m) -> m (Perspective, [State (Game m)])
getPerspectiveAndDescendants s = do
  g    <- askGame
  self <- askSelf
  let currentTurn   = turn g s
      myPerspective = if currentTurn == self then Self else Other
      descendants   = map (\m -> makeMove g m s) (availableMoves g s)
  return (myPerspective, descendants)

recompute :: MonadSolveGame m => State (Game m) -> m ()
recompute s = do
  (myPerspective, descendants) <- getPerspectiveAndDescendants s
  rs                           <- mapM lookupDefaultedState descendants
  writeState s (combineResults myPerspective rs)
