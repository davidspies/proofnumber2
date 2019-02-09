module TicTacToe
  ( Game(..)
  , Piece(..)
  , Position(..)
  , State(..)
  , solve
  )
where

import           DSpies.Prelude          hiding ( State )

import qualified Data.Map                      as Map

data Piece = X | O
  deriving (Eq, Show)
data Position = Position {row :: Int, col :: Int}
  deriving (Eq, Ord, Show)

data State = State {board :: Map Position Piece, turn :: Piece}
  deriving (Show)

data Game = Game {nrows :: Int, ncols :: Int, inARow :: Int}

positions :: Game -> [Position]
positions Game {..} =
  [ Position { .. } | row <- [1 .. nrows], col <- [1 .. ncols] ]

newtype GameResult = GameResult {winner :: Maybe Piece}
  deriving (Show)

data Outcome = Lose | Tie | Win
  deriving (Eq, Ord)

allSameJust :: (HasCallStack, Eq a) => [Maybe a] -> Maybe a
allSameJust []            = error "Empty list"
allSameJust (Nothing : _) = Nothing
allSameJust (Just x0 : xs) | all (== Just x0) xs = Just x0
                           | otherwise           = Nothing

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn fn = maximumBy (\x y -> compare (fn x) (fn y))

oppositePlayer :: Piece -> Piece
oppositePlayer = \case
  X -> O
  O -> X

solve :: IsGame g => g -> StateOf g -> ResultOf g
solve game = go
 where
  go s = fromMaybe continue (status game s)
   where
    continue = maximumOn (outcome game (turnOf game s))
      $ map (\p -> go (makeMove game p s)) (availableMoves game s)

class Ord (OutcomeOf g) => IsGame g where
  type StateOf g
  type ResultOf g
  type OutcomeOf g
  type MoveOf g
  type PlayerOf g

  makeMove :: g -> MoveOf g -> StateOf g -> StateOf g
  status :: g -> StateOf g -> Maybe (ResultOf g)
  outcome :: g -> PlayerOf g -> ResultOf g -> OutcomeOf g
  availableMoves :: g -> StateOf g -> [MoveOf g]
  turnOf :: g -> StateOf g -> PlayerOf g

instance IsGame Game where
  type StateOf Game = State
  type ResultOf Game = GameResult
  type OutcomeOf Game = Outcome
  type MoveOf Game = Position
  type PlayerOf Game = Piece

  makeMove _ p State {..} =
    State { board = Map.insert p turn board, turn = oppositePlayer turn }

  outcome _ _ (GameResult Nothing) = Tie
  outcome _ p (GameResult (Just q)) | p == q    = Win
                                    | otherwise = Lose

  availableMoves g State { board } =
    [ p | p <- positions g, not $ p `Map.member` board ]

  turnOf _ State { turn } = turn

  status g@Game {..} s@State { board } = case winner of
    Just _                              -> Just $ GameResult { winner }
    Nothing | null (availableMoves g s) -> Just $ GameResult { winner }
    Nothing                             -> Nothing
   where
    allSamePiece ps = allSameJust $ map (`Map.lookup` board) ps
    eastWinAt r0 c0 = allSamePiece
      [ Position { row = r0, col = c0 + d } | d <- [0 .. inARow - 1] ]
    southWinAt r0 c0 = allSamePiece
      [ Position { row = r0 + d, col = c0 } | d <- [0 .. inARow - 1] ]
    southEastWinAt r0 c0 = allSamePiece
      [ Position { row = r0 + d, col = c0 + d } | d <- [0 .. inARow - 1] ]
    southWestWinAt r0 c0 = allSamePiece
      [ Position { row = r0 - d, col = c0 + d } | d <- [0 .. inARow - 1] ]
    winner =
      listToMaybe
        $  catMaybes
        $  [ eastWinAt startR startC
           | startR <- [1 .. nrows]
           , startC <- [1 .. (ncols - inARow + 1)]
           ]
        ++ [ southWinAt startR startC
           | startR <- [1 .. (nrows - inARow + 1)]
           , startC <- [1 .. ncols]
           ]
        ++ [ southEastWinAt startR startC
           | startR <- [1 .. (nrows - inARow + 1)]
           , startC <- [1 .. (ncols - inARow + 1)]
           ]
        ++ [ southWestWinAt startR startC
           | startR <- [inARow .. nrows]
           , startC <- [1 .. (ncols - inARow + 1)]
           ]
