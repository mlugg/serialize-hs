module Data.Serialize where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

newtype Serialize s a = Serialize { runSerializer :: a -> s }

instance Contravariant (Serialize s) where
  contramap f (Serialize g) = Serialize (g . f)

instance (Monoid s) => Divisible (Serialize s) where
  divide f (Serialize g0) (Serialize g1) = Serialize $ \x ->
    case f x of
      (y0, y1) -> g0 y0 <> g1 y1

  conquer = Serialize $ const mempty

instance (Monoid s) => Decidable (Serialize s) where
  lose f = Serialize $ absurd . f
  choose f (Serialize g0) (Serialize g1) = Serialize $ \x ->
    case f x of
      Left y -> g0 y
      Right z -> g1 z
