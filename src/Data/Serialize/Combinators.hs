{-|

Module     : Data.Serialize.Combinators
Copyright  : © Matthew Lugg, 2020
License    : Unlicense
Maintainer : mlugg@mlugg.co.uk
Stability  : experimental

This module defines a series of simple combinators for contravariant
functors. Some or all of these should really be in
'Data.Functor.Contravariant.Divisible'.

-}

{-# LANGUAGE Safe, LambdaCase #-}

module Data.Serialize.Combinators
  ( module Data.Serialize.Combinators
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible) where

-- The definitions in this module should really be in the
-- 'contravariant' package.

import Data.Serialize
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

-- |@'list' f@ repeats @f@ for every element of a list.
list :: (Decidable f) => f a -> f [a]
list x = go
  where
    go = choose uncons conquered (divided x go)
    uncons = \case
      [] -> Left ()
      x:xs -> Right (x,xs)

-- |@f `'sepBy'` s@ repeats @f@ for every element of a list, but also
-- runs @s@ between every invocation (using the trivial unit value
-- @()@).
sepBy :: (Decidable f) => f a -> f () -> f [a]
sepBy x s = choose uncons conquered (divided x (list $ s *< x))
  where
    uncons = \case
      [] -> Left ()
      x:xs -> Right (x,xs)

-- |@'between' l r x@ runs @l@ with the unit value @()@, then @x@ with
-- the given input, then @r@ with the unit value @()@.
between :: (Divisible f) => f () -> f () -> f a -> f a
between l r x = l *< x >* r


-- |An infix synonym for 'divided'.
infixr 4 >*<
(>*<) :: (Divisible f) => f a -> f b -> f (a, b)
(>*<) = divided

-- |An infix synonym for 'chosen'.
infixr 3 >|<
(>|<) :: (Decidable f) => f a -> f b -> f (Either a b)
(>|<) = chosen

-- |@x '>*' y@ runs @x@ with the given input and @y@ with the unit value
-- @()@.
infixr 4 >*
(>*) :: (Divisible f) => f a -> f () -> f a
(>*) = divide (\x -> (x,()))

-- |@x '*<' y@ runs @x@ with the unit value @()@ and @y@ with the given
-- input.
infixr 4 *<
(*<) :: (Divisible f) => f () -> f a -> f a
(*<) = divide (\x -> ((),x))
