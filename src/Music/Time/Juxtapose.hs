
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Juxtapose (
        -- * Prerequisites
        Transformable(..),

        -- ** Juxtaposing values
        following,
        preceding,
        during,

        -- * Composing values
        (|>),
        (>|),
        (<|),
        scat,
        pcat,

        -- ** Special composition
        sustain,
        anticipate,

        -- ** Repetition
        times,
        repeated,
        group,
  ) where


import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.VectorSpace

import           Music.Time.Delayable
import           Music.Time.Onset
import           Music.Time.Stretchable
import           Music.Time.Time

-- |
-- This pseudo-class gathers the restrictions needed to implement position a value at
-- any point and duration in time.
--
type Transformable a   =  (Stretchable a, Delayable a)


-------------------------------------------------------------------------------------
-- Juxtaposition
-------------------------------------------------------------------------------------

-- |
-- @a \`following\` b@ moves score /b/ so that its onset is at the offset of score
-- /a/ and returns the moved score.
--
following :: (HasOffset a, Delayable b, HasOnset b) => a -> b -> b
a `following` b =  startAt (offset a) b

-- |
-- @a \`preceding\` b@ moves score /a/ so that its offset is at the onset of score
-- /b/ and returns the moved score.
--
preceding :: (Delayable a, HasOffset a, HasOnset b) => a -> b -> a
a `preceding` b =  stopAt (onset b) a

-- | @a \`during\` b@ places /a/ at the same era as /b/ and returns the moved score.
during :: (Delayable a, Stretchable a, HasOnset a, HasDuration a, HasOnset b, HasDuration b) => a -> b -> a
a `during` b = startAt (onset b) $ stretchTo (duration b) a

-------------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------------

infixr 6 |>
infixr 6 >|
infixr 6 <|

-- |
-- Compose in sequence.
--
-- @a |> b@ moves score /b/ as per 'following' and then composes the resulting scores with '<>'.
--
-- > Score a -> Score a -> Score a
--
(|>)            :: (Semigroup a, HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
-- |
-- Compose in sequence.
--
-- @a >| b@ moves score /a/ as per 'preceding' and then composes the resulting scores with '<>'.
--
-- > Score a -> Score a -> Score a
--
(>|)            :: (Semigroup a, HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

-- |
-- Compose in reverse sequence.
--
-- To compose in parallel, use '<>'.
--
-- > Score a -> Score a -> Score a
--
(<|)            :: (Semigroup a, HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

a |> b =  a <> (a `following` b)
a >| b =  (a `preceding` b) <> b
a <| b =  b |> a

-- |
-- Sequential catenation.
--
-- > [Score a] -> Score a
--
scat            :: (Monoid' a, HasOnset a, HasOffset a, Delayable a) =>
                [a] -> a
-- |
-- Parallel catenation.
--
-- > [Score a] -> Score a
--
pcat            :: Monoid' a =>
                [a] -> a

scat = Prelude.foldr (|>) mempty
pcat = Prelude.foldr (<>) mempty


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


-- --------------------------------------------------------------------------------
-- -- Structure
-- --------------------------------------------------------------------------------

-- |
-- Repeat exact amount of times.
--
-- > Duration -> Score Note -> Score Note
--
times           :: (Monoid' a, Transformable a, HasOnset a, HasOffset a) =>
                Int -> a -> a

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

