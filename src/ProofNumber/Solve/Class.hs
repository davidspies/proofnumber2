module ProofNumber.Solve.Class
  ( MonadSolveGame(..)
  , Node
  , maximumOn
  , solve
  )
where

import           DSpies.Prelude          hiding ( State )

import           ProofNumber.Game

data Node = Finished Bool | Remaining Costs

data Costs = Costs {selfCost :: Integer, othersCost :: Integer}

class (IsGame (Game m), Eq (Player (Game m)), Monad m) => MonadSolveGame m where
  type Game m
  askSelf :: m (Player (Game m))
  askGame :: m (Game m)
  askDesired :: m (Outcome (Game m))
  lookupState :: State (Game m) -> m (Maybe Node)
  writeState :: State (Game m) -> Node -> m ()

solve :: forall m . MonadSolveGame m => State (Game m) -> m Bool
solve s0 = do
  let go = lookupDefaultedState s0 >>= \case
        Finished b  -> return b
        Remaining{} -> do
          expand s0
          go
  go

data Perspective = Self | Other

opposite :: Perspective -> Perspective
opposite = \case
  Self  -> Other
  Other -> Self

data Infinible a = Finite a | Infinity
  deriving (Eq, Ord)

instance Num a => Num (Infinible a) where
  (+) Infinity   _          = Infinity
  (+) _          Infinity   = Infinity
  (+) (Finite x) (Finite y) = Finite (x + y)

  (*) Infinity   _          = Infinity
  (*) _          Infinity   = Infinity
  (*) (Finite x) (Finite y) = Finite (x * y)

  abs (Finite x) = Finite $ abs x
  abs Infinity   = Infinity

  signum (Finite x) = Finite $ signum x
  signum Infinity   = Finite 1

  negate (Finite x) = Finite $ negate x
  negate Infinity   = error "Cannot negate Infinity"

  fromInteger = Finite . fromInteger

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

lookupDefaultedState :: MonadSolveGame m => State (Game m) -> m Node
lookupDefaultedState s = do
  g <- askGame
  case status g s of
    Nothing -> fromMaybe (Remaining (Costs 1 1)) <$> lookupState s
    Just r  -> do
      self    <- askSelf
      desired <- askDesired
      return $ Finished $ outcome g self r >= desired

minimumOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m a
minimumOnM fn xs =
  fst
    .   minimumBy (\x y -> compare (snd x) (snd y))
    <$> mapM (\x -> (x, ) <$> fn x) xs

expand :: MonadSolveGame m => State (Game m) -> m ()
expand s = lookupState s >>= \case
  Nothing          -> recompute s
  Just Finished{}  -> return ()
  Just Remaining{} -> do
    (myPerspective, descendants) <- getPerspectiveAndDescendants s
    chosen                       <- minimumOnM
      (fmap (proofCost myPerspective) . lookupDefaultedState)
      descendants
    expand chosen
    recompute s

makeNode :: Perspective -> Infinible Integer -> Infinible Integer -> Node
makeNode Self  Infinity            _                   = Finished False
makeNode Self  _                   Infinity            = Finished True
makeNode Other Infinity            _                   = Finished True
makeNode Other _                   Infinity            = Finished False
makeNode Self  (Finite selfCost  ) (Finite othersCost) = Remaining Costs { .. }
makeNode Other (Finite othersCost) (Finite selfCost  ) = Remaining Costs { .. }

combineResults :: Perspective -> [Node] -> Node
combineResults p ns = makeNode p pCost oppCost
 where
  pCost   = minimum $ map (proofCost p) ns
  oppCost = sum $ map (proofCost (opposite p)) ns

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

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn fn =
  fst . maximumBy (\x y -> compare (snd x) (snd y)) . map (id &&& fn)
