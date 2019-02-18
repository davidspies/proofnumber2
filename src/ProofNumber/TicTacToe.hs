module ProofNumber.TicTacToe
  ( Dimensions(..)
  , Game(..)
  , GameResult
  , Outcome(..)
  , Piece(..)
  , State
  , emptyState
  )
where

import           DSpies.Prelude          hiding ( State )

import           ProofNumber.Game               ( IsGame )
import qualified ProofNumber.Game              as Game
import           ProofNumber.TicTacToe.Board   as Board

data StateKernel = StateKernel
  { board :: Board
  , turn :: Piece
  }
  deriving (Eq, Ord, Show)

data State = State
  { kernel :: StateKernel
  , nextMoves :: [Position]
  }

instance Eq State where
  (==) x y = kernel x == kernel y
instance Ord State where
  compare x y = compare (kernel x) (kernel y)
instance Show State where
  showsPrec d State {..} =
    showParen (d > 10) $ showString "state g " . showsPrec 11 kernel

state :: Game -> StateKernel -> State
state g kernel@StateKernel { board } = State
  { kernel
  , nextMoves = [ p | p <- positions (dims g), not $ Board.member p board ]
  }

data Game = Game {dims :: Dimensions, inARow :: Int}

newtype GameResult = GameResult {winner :: Maybe Piece}
  deriving (Show)

data Outcome = Lose | Tie | Win
  deriving (Eq, Ord)

allSameJust :: (HasCallStack, Eq a) => [Maybe a] -> Maybe a
allSameJust []            = error "Empty list"
allSameJust (Nothing : _) = Nothing
allSameJust (Just x0 : xs) | all (== Just x0) xs = Just x0
                           | otherwise           = Nothing

oppositePlayer :: Piece -> Piece
oppositePlayer = \case
  X -> O
  O -> X

emptyState :: Game -> State
emptyState g =
  let board = Board.empty (dims g) in state g StateKernel { board, turn = X }

instance IsGame Game where
  type State Game = State
  type Result Game = GameResult
  type Outcome Game = Outcome
  type Move Game = Position
  type Player Game = Piece

  makeMove g p State { kernel = StateKernel {..} } = state
    g
    StateKernel { board = Board.insert p turn board
                , turn  = oppositePlayer turn
                }

  outcome _ _ (GameResult Nothing) = Tie
  outcome _ p (GameResult (Just q)) | p == q    = Win
                                    | otherwise = Lose

  availableMoves _ = nextMoves

  turn _ State { kernel = StateKernel { turn } } = turn

  status Game { dims = dims@Dimensions {..}, inARow } s@State { kernel = StateKernel { board } }
    = case winner of
      Just _                       -> Just $ GameResult { winner }
      Nothing | null (nextMoves s) -> Just $ GameResult { winner }
      Nothing                      -> Nothing
   where
    allSamePiece ps = allSameJust $ map (\x -> Board.lookup x board) ps
    eastWinAt r0 c0 =
      allSamePiece [ mkPos dims r0 (c0 + d) | d <- [0 .. inARow - 1] ]
    southWinAt r0 c0 =
      allSamePiece [ mkPos dims (r0 + d) c0 | d <- [0 .. inARow - 1] ]
    southEastWinAt r0 c0 =
      allSamePiece [ mkPos dims (r0 + d) (c0 + d) | d <- [0 .. inARow - 1] ]
    southWestWinAt r0 c0 =
      allSamePiece [ mkPos dims (r0 - d) (c0 + d) | d <- [0 .. inARow - 1] ]
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
