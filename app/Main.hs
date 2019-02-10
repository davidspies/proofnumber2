module Main where

import           DSpies.Prelude

import           Control.Monad.Logger          as Logger

import           ProofNumber.Solve
import           ProofNumber.TicTacToe         as TicTacToe

main :: IO ()
main = do
  let game = TicTacToe.Game 3 3 3
  res <- runSolver Logger.LevelInfo game X Tie
    $ solve (TicTacToe.emptyState game)
  print res
