{-# LANGUAGE LambdaCase #-}

module Data.Serialize.Combinators
  ( module Data.Serialize.Combinators
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible) where

import Data.Serialize
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

list :: (Decidable f) => f a -> f [a]
list x = go
  where
    go = choose uncons conquered (divided x go)
    uncons = \case
      [] -> Left ()
      x:xs -> Right (x,xs)

sepBy :: (Decidable f) => f a -> f () -> f [a]
sepBy x s = choose uncons conquered (divided x (list $ s *< x))
  where
    uncons = \case
      [] -> Left ()
      x:xs -> Right (x,xs)

between :: (Divisible f) => f () -> f () -> f a -> f a
between l r x = l *< x >* r

-- Why doesn't Data.Functor.Contravariant.Divisible define these? :-(

infixr 4 >*<
(>*<) :: (Divisible f) => f a -> f b -> f (a, b)
(>*<) = divided

infixr 3 >|<
(>|<) :: (Decidable f) => f a -> f b -> f (Either a b)
(>|<) = chosen

infixr 4 >*
(>*) :: (Divisible f) => f a -> f () -> f a
(>*) = divide (\x -> (x,()))

infixr 4 *<
(*<) :: (Divisible f) => f () -> f a -> f a
(*<) = divide (\x -> ((),x))
