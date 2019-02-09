module ProofNumber.Solve.Class
  ( MonadSolveGame(..)
  , maximumOn
  , solve
  )
where

import           DSpies.Prelude          hiding ( State )

import           ProofNumber.Game

class (IsGame (Game m), Monad m) => MonadSolveGame m where
  type Game m
  askGame :: m (Game m)
  lookupState :: State (Game m) -> m (Maybe (Result (Game m)))
  writeState :: State (Game m) -> Result (Game m) -> m ()

solve :: MonadSolveGame m => State (Game m) -> m (Result (Game m))
solve s0 = lookupState s0 >>= \case
  Just res -> pure res
  Nothing  -> do
    game <- askGame
    let go s = case status game s of
          Just res -> pure res
          Nothing  -> maximumOn (outcome game (turn game s))
            <$> mapM (\p -> go (makeMove game p s)) (availableMoves game s)
    result <- go s0
    writeState s0 result
    pure result

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn fn =
  fst . maximumBy (\x y -> compare (snd x) (snd y)) . map (id &&& fn)
