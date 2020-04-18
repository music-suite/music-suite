{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  #-}

module Music.Time.Juxtapose
  ( module Music.Time.Split,

    -- * Align without composition
    lead,
    follow,

    -- * Standard composition
    after,
    before,
    during,
    (|>),
    (<|),

    -- ** More exotic
    rev,
    sustain,
    palindrome,

    -- * Catenation
    pseq,
    ppar,

    -- * Repetition
    times,
    group,
    After (..),
  )
where

import Control.Lens hiding ((<|), (|>))
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Semigroup
import Data.Stream.Infinite hiding (group)
import Data.VectorSpace
import GHC.Generics (Generic)
import Music.Time.Split

-- |
-- @
-- (a `lead` b)^.'offset' = b^.'onset'
-- @
lead :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b = case b `_position` 0 of
  Nothing -> a
  Just p -> placeAt 1 p a

-- |
-- @
-- a^.'offset' = (a `follow` b)^.'onset'
-- @
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = case a `_position` 1 of
  Nothing -> b
  Just p -> placeAt 0 p b

after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b = a <> (a `follow` b)

before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b = (a `lead` b) <> b

-- |
-- A value followed by its reverse (retrograde).
palindrome :: (Semigroup a, Transformable a, HasPosition a) => a -> a
palindrome a = a `after` rev a

rev :: (Transformable a, HasPosition a) => a -> a
rev = stretchRelativeMidpoint (-1)

infixr 6 |>

infixr 6 <|

-- |
-- An infix alias for 'after'.
(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

-- |
-- An infix alias for 'before'.
(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

-- infixr 6 >|
-- infixr 6 |<

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
pseq :: (Monoid a, HasPosition a, Transformable a) => [a] -> a
pseq = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- For positioned types this is the same as 'mconcat'.
ppar :: (Monoid a) => [a] -> a
ppar = mconcat

-- Though (ppar = mconcat), the extra constraints prevents ppar from being used on sequential
-- types such as Voice.

-- |
-- Move a value so that its era is equal to the era of another value.
during :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
y `during` x = case _era x of
  Nothing -> y
  Just e -> setEra e y

-- |
-- Like '<>', but scaling the second agument to the duration of the first.
sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y = x <> y `during` x

-- |
-- Repeat the given music /n/ times.
--
-- @
-- 'Int' -> 'Score' a -> 'Score' a
-- @
times :: (Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n = pseq . replicate n

-- |
-- Repeat the given music /n/ times and stretch to the original duration.
--
-- @
-- 'Int' -> 'Score' a -> 'Score' a
-- @
group :: (Monoid a, Transformable a, HasPosition a) => Int -> a -> a
group n x = times n x |/ fromIntegral n

{-
-- |
-- Compose sequentially by aligning the nominal position of each value to the
-- first available time value.
--
-- TODO this requires another constraint for nominal position. For (Aligned ((t,_),_))
-- the nominal position is t.
--
-- @
-- length xs = length (snapTo ts xs)
-- @
snapTo :: (HasPosition a, Transformable a) => Stream Time -> [a] -> [a]
-}

-- | Monoid under sequential composition.
--
-- /Warning/: This is only lawful if the underlying monoid means "parallel composition".
-- E.g. this works for 'Score' and 'Pattern', but not for 'Span' and 'Voice'.
newtype After a = After {getAfter :: a}
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

instance (Semigroup a, HasPosition a, Transformable a) => Semigroup (After a) where
  After a <> After b = After $ a |> b

instance (Monoid a, HasPosition a, Transformable a) => Monoid (After a) where
  mempty = After mempty
