module Main where

import           DSpies.Prelude

import           ProofNumber.Solve.Class
import           ProofNumber.Solve.Impl
import qualified ProofNumber.TicTacToe         as TicTacToe

main :: IO ()
main = do
  let game = TicTacToe.Game 3 3 3
  res <- runSolver game $ solve (TicTacToe.emptyState game)
  print res
