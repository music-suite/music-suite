
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving #-} 

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
        Monoid'(..),
        Transformable(..),

        -- ** Juxtaposing values
        follow,
        precede,

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


import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Time.Onset

-- |
-- This pseudo-class can be used in place of 'Monoid' whenever an additional 'Semigroup'
-- constraint is needed.
--
-- Ideally, 'Monoid' should be changed to extend 'Semigroup' instead.
--
type Monoid' a         =  (Monoid a, Semigroup a)

type Transformable a   =  (Stretchable a, Delayable a, AffineSpace (Time a))


-------------------------------------------------------------------------------------
-- Juxtaposition
-------------------------------------------------------------------------------------

-- |
-- @a \`follow\` b@ moves score /b/ so that its onset is at the offset of score
-- /a/ and returns the moved score.
-- 
follow           :: (AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

-- |
-- @a \`precede\` b@ moves score /a/ so that its offset is at the onset of score
-- /b/ and returns the moved score.
-- 
precede          :: (AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

a `follow` b =  startAt (offset a) b

a `precede` b =  stopAt (onset b) a


-------------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------------

infixr 6 |>
infixr 6 >|
infixr 6 <|

-- |
-- Compose in sequence.
--
-- @a |> b@ moves score /b/ as per 'follow' and then composes the resulting scores with '<>'.
--
-- > Score a -> Score a -> Score a
--
(|>)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
-- |
-- Compose in sequence.
--
-- @a >| b@ moves score /a/ as per 'precede' and then composes the resulting scores with '<>'.
--
-- > Score a -> Score a -> Score a
--
(>|)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

-- |
-- Compose in reverse sequence.
--
-- To compose in parallel, use '<>'.
--
-- > Score a -> Score a -> Score a
--
(<|)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a

a |> b =  a <> (a `follow` b)
a >| b =  (a `precede` b) <> b
a <| b =  b |> a

-- |
-- Sequential catenation.
--
-- > [Score a] -> Score a
--
scat            :: (Monoid' a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
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
sustain         :: (Semigroup a, Stretchable a, HasDuration a, Fractional d, d ~ Duration a) =>
                a -> a -> a

-- |
-- Like '|>' but with a negative delay on the second element.
--
-- > Duration -> Score a -> Score a -> Score a
--
anticipate      :: (Semigroup a, Transformable a, HasOnset a, HasOffset a, Ord d, d ~ Duration a) =>
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
group           :: (Monoid' a, Transformable a, Fractional d, d ~ Duration a, HasOnset a, HasOffset a) =>
                Int -> a -> a

times n     = scat . replicate n
repeated    = flip (\f -> scat . fmap f)
group n     = times n . (fromIntegral n `compress`)
