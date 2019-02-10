module Main where

import           DSpies.Prelude

import           Control.Monad.Logger          as Logger

import           ProofNumber.Solve.Class
import           ProofNumber.Solve.Impl
import qualified ProofNumber.TicTacToe         as TicTacToe

main :: IO ()
main = do
  let game = TicTacToe.Game 3 3 3
  res <- runSolver Logger.LevelInfo game TicTacToe.X TicTacToe.Tie
    $ solve (TicTacToe.emptyState game)
  print res
