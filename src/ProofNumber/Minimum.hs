module ProofNumber.Minimum
    ( minimumOnA
    ) where

import DSpies.Prelude

minimumOnA :: (Applicative f, Ord b) => (a -> f b) -> [a] -> f a
minimumOnA fn xs =
  fst
    .   minimumBy (\x y -> compare (snd x) (snd y))
    <$> traverse (\x -> (x, ) <$> fn x) xs
