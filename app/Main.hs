module Main where

import           DSpies.Prelude

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Logger          as Logger
import           UnliftIO

import           ProofNumber.Solve
import           ProofNumber.TicTacToe         as TicTacToe

main :: IO ()
main = do
  let game = TicTacToe.Game 3 3 3
  res <-
    runSolver Logger.LevelInfo game X Win
    $ withAsync (liftIO $ threadDelay 1000000)
    $ \e -> solve e (TicTacToe.emptyState game)
  print res
