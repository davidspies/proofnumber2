{-# LANGUAGE TemplateHaskell #-}

module ProofNumber.Solve.Internal
  ( Analysis(..)
  , solve
  )
where

import           DSpies.Prelude          hiding ( State )

import           Control.Monad.Logger
import qualified Data.Text                     as Text

import           ProofNumber.AnyTime.Class
import           ProofNumber.Game
import           ProofNumber.Minimum
import           ProofNumber.Solve.Class
import           ProofNumber.Solve.Node

type ChildMap move = [(move, Double)]

data Analysis move = Analysis
  { value :: Double
  , children :: ChildMap move
  }
  deriving (Show)

solve
  :: (MonadSolveGame m, MonadAnyTime m)
  => Event m
  -> State (Game m)
  -> m (Analysis (Move (Game m)))
solve e s0 = runUntilCancelledAndFinalize e $ do
  expand s0
  lookupDefaultedState s0 <&> \case
    Finished b ->
      ( do
        children <- lookupChildren s0
        return Analysis { value = if b then 1 else 0, children }
      , True
      )
    Remaining Costs { selfProb } ->
      ( do
        children <- lookupChildren s0
        return Analysis { value = selfProb, children }
      , False
      )

lookupChildren
  :: MonadSolveGame m => State (Game m) -> m (ChildMap (Move (Game m)))
lookupChildren s = do
  g <- askGame
  forM (availableMoves g s) $ \m -> (m, ) <$> lookupStateProb (makeMove g m s)

lookupStateProb :: MonadSolveGame m => State (Game m) -> m Double
lookupStateProb s = lookupDefaultedState s <&> \case
  Finished  b                  -> if b then 1 else 0
  Remaining Costs { selfProb } -> selfProb

lookupDefaultedState :: MonadSolveGame m => State (Game m) -> m Node
lookupDefaultedState s = do
  g <- askGame
  case status g s of
    Nothing -> lookupState s >>= \case
      Nothing -> do
        selfProb <- heuristic s
        return $ Remaining $ Costs { selfCost   = -(log selfProb)
                                   , othersCost = -(log (1 - selfProb))
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
