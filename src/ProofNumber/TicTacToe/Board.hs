module ProofNumber.TicTacToe.Board
  ( Board
  , Dimensions(..)
  , Piece(..)
  , Position
  , empty
  , insert
  , lookup
  , member
  , mkPos
  , positions
  )
where

import           Data.Bits

import           DSpies.Prelude          hiding ( empty
                                                , insert
                                                , lookup
                                                )

data Piece = X | O
  deriving (Eq, Ord, Show)
newtype Position = Position Int
  deriving (Eq, Ord, Show)
data Dimensions = Dimensions {nrows :: Int, ncols :: Int}
data Board = Board
  { xsBits :: Integer
  , osBits :: Integer
  }
  deriving (Eq, Ord, Show)

adjustSide :: Piece -> (Integer -> Integer) -> Board -> Board
adjustSide p fn Board {..} = case p of
  X -> Board { xsBits = fn xsBits, osBits }
  O -> Board { osBits = fn osBits, xsBits }

empty :: Dimensions -> Board
empty _ = Board 0 0

insert :: Position -> Piece -> Board -> Board
insert (Position b) piece = adjustSide piece (\bits -> bits .|. bit b)

lookup :: Position -> Board -> Maybe Piece
lookup (Position b) Board {..} | testBit xsBits b = Just X
                               | testBit osBits b = Just O
                               | otherwise        = Nothing

member :: Position -> Board -> Bool
member = isJust .: lookup

mkPos :: Dimensions -> Int -> Int -> Position
mkPos Dimensions {..} row col = Position $ (row - 1) * ncols + col - 1

positions :: Dimensions -> [Position]
positions Dimensions {..} = map Position [0 .. nrows * ncols - 1]
