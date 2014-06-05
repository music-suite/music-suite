
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Juxtapose (
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
        sustain,
        palindrome,

        -- * Catenation
        scat,
        pcat,

        -- * Repetition
        times,
  ) where

import           Control.Lens hiding ((|>), (<|))
import           Data.AffineSpace
import           Data.AffineSpace.Point
-- import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.VectorSpace

import           Music.Time.Reverse
import           Music.Time.Split


--
-- TODO names
-- Especially 'after' is counter-intuitive
--

-- |
-- Move a value so that
--
-- @
-- '_offset' (a ``lead`` b) = '_onset' b
-- @
--
--
lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = placeAt 1 (b `_position` 0) a

-- |
-- Move a value so that
--
-- @
-- '_offset' a = '_onset' (a ``follow`` b)
-- @
--
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

-- |
-- Move a value so that
--
after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

-- |
-- Move a value so that
--
before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

-- |
-- A value followed by its reverse (retrograde).
--
palindrome :: (Semigroup a, Reversible a, HasPosition a) => a -> a
palindrome a = a `after` rev a

infixr 6 |>
infixr 6 <|

-- |
-- An infix alias for 'after'.
--
(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

-- |
-- An infix alias for 'before'.
--
(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

-- infixr 6 >|
-- infixr 6 |<

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
--
-- For non-positioned types, this is the often same as 'mconcat'
-- For positioned types, this is the same as 'afterAnother'
--
scat :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
scat = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- This not possible for non-positioned types, as they have no notion of an origin.
-- For positioned types this is the same as 'mconcat'.
--
pcat :: (Semigroup a, Monoid a) => [a] -> a
pcat = Prelude.foldr (<>) mempty

-- |
-- Move a value so that its era is equal to the era of another value.
--
-- @
-- 'Score' a -> 'Score' a -> 'Score' a
-- @
--
during :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
y `during` x = set era (view era x) y

-- |
-- Like '<>', but scaling the second agument to the duration of the first.
--
-- @
-- 'Score' a -> 'Score' a -> 'Score' a
-- @
--
sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y = x <> y `during` x

-- |
-- Repeat exact amount of times.
--
-- @
-- 'Int' -> 'Score' a -> 'Score' a
-- @
--
times :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n = scat . replicate n

{-

-- |
-- Like '<>', but scaling the second agument to the duration of the first.
--
-- > Score a -> Score a -> Score a
--
sustain         :: (Semigroup a, Stretchable a, HasDuration a, Fractional d, d ~ Duration) =>
                a -> a -> a

-- |
-- Like '|>' but with a negative delay on the second element.
--
-- > Duration -> Score a -> Score a -> Score a
--
anticipate      :: (Semigroup a, Transformable a, HasOnset a, HasOffset a, Ord d, d ~ Duration) =>
                d -> a -> a -> a

x `sustain` y     = x <> duration x `stretchTo` y
anticipate t a b  =  a <> startAt (offset a .-^ t) b

-- Like '<>', but truncating the second agument to the duration of the first.
-- prolong x y = x <> before (duration x) y

-- |
-- Repeat once for each element in the list.
--
-- Example:
--
-- > repeated [1,2,1] (c^*)
--
-- Simple type:
--
-- > [a] -> (a -> Score Note) -> Score Note
--
repeated        :: (Monoid' b, Transformable b, HasOnset b, HasOffset b) =>
                [a] -> (a -> b) -> b

-- |
-- Repeat a number of times and scale down by the same amount.
--
-- > Duration -> Score a -> Score a
--
group           :: (Monoid' a, Transformable a, Fractional d, d ~ Duration, HasOnset a, HasOffset a) =>
                Int -> a -> a

times n     = scat . replicate n
repeated    = flip (\f -> scat . fmap f)
group n     = times n . (fromIntegral n `compress`)
-}

