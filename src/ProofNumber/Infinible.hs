module ProofNumber.Infinible where

import           DSpies.Prelude

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
