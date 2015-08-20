
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE DeriveDataTypeable         #-}
-- {-# LANGUAGE DeriveFoldable             #-}
-- {-# LANGUAGE DeriveFunctor              #-}
-- {-# LANGUAGE DeriveTraversable          #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}

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

module Music.Time.Aligned (
      -- * Alignable class
      Alignable(..),
      -- * Aligned values
      Aligned,
      aligned,
      realign,
      renderAligned,
      renderAlignedVoice,
      renderAlignedNote,
      renderAlignedDuration,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens            hiding (Indexable, Level, above, below,
                                          index, inside, parts, reversed,
                                          transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Foldable           (Foldable)
import qualified Data.Foldable           as Foldable
import           Data.Functor.Adjunction (unzipR)
import           Data.Functor.Couple
import           Data.String
import           Data.Typeable
import           Data.VectorSpace
import           Data.Aeson                    (ToJSON (..))
import qualified Data.Aeson                    as JSON

import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import           Music.Time.Event
import           Music.Time.Juxtapose
import           Music.Time.Note
import           Music.Time.Score
import           Music.Time.Voice

-- TODO should this be a Setter or a Lens instead?
class Alignable a where
  align :: Alignment -> a -> a

instance Alignable a => Alignable [a] where
  align l = fmap (align l)

instance Alignable (Aligned a) where
  align l (Aligned ((t, _), a)) = Aligned ((t, l), a)

-- type AlignedVoice a = Aligned (Voice a)

-- | 'Aligned' places a vector-like object in space, by fixing a local duration interpolating
-- the vector to a specific point in time. The aligned value must be an instance of
-- 'HasDuration', with @'view' 'duration'@ providing the size of the vector.
--
-- This is analogous to alignment in a graphical program. To align something at onset, midpoint
-- or offset, use 0, 0.5 or 1 as the local duration value.
newtype Aligned v = Aligned { getAligned :: ((Time, Alignment), v) }
  deriving (Functor, Eq, Ord, Foldable, Traversable)

instance Wrapped (Aligned v) where
  type Unwrapped (Aligned v) = ((Time, Alignment), v)
  _Wrapped' = iso getAligned Aligned

instance Rewrapped (Aligned a) (Aligned b)

-- | Align the given value so that its local duration occurs at the given time.
aligned :: Time -> Alignment -> v -> Aligned v
aligned t d a = Aligned ((t, d), a)

instance Show a => Show (Aligned a) where
  show (Aligned ((t,d),v)) = "aligned ("++show t++") ("++show d++") ("++ show v++")"

instance ToJSON a => ToJSON (Aligned a) where
  toJSON (Aligned ((t,d),v)) = JSON.object [ ("alignment", toJSON d), ("origin", toJSON t), ("value", toJSON v) ]

instance Transformable v => Transformable (Aligned v) where
  transform s (Aligned ((t, d), v)) = Aligned ((transform s t, d), transform s v)

instance (HasDuration v, Transformable v) => HasDuration (Aligned v) where
  _duration (Aligned (_, v)) = v^.duration

instance (HasDuration v, Transformable v) => HasPosition (Aligned v) where
  -- _position (Aligned (position, alignment, v)) = alerp (position .-^ (size * alignment)) (position .+^ (size * (1-alignment)))
  _era (Aligned ((position, alignment), v)) =
    (position .-^ (size * alignment)) <-> (position .+^ (size * (1-alignment)))
    where
      size = v^.duration

-- | Change the alignment of a value without moving it.
--
-- @
-- x^.'era' = ('realign' l x)^.'era'
-- @
realign :: (HasDuration a, Transformable a) => Alignment -> Aligned a -> Aligned a
realign l a@(Aligned ((t,_),x)) = Aligned ((a^.position l,l),x)

-- | Render an aligned value. The given span represents the actual span of the aligned value.
renderAligned :: (HasDuration a, Transformable a) => (Span -> a -> b) -> Aligned a -> b
renderAligned f a@(Aligned (_, v)) = f (_era a) v


-- Somewhat suspect, see below for clarity...

voiceToScoreInEra :: Span -> Voice a -> Score a
voiceToScoreInEra e = set era e . scat . map (uncurry stretch) . view pairs . fmap pure

noteToEventInEra :: Span -> Note a -> Event a
noteToEventInEra e = set era e . view notee . fmap pure

durationToSpanInEra :: Span -> Duration -> Span
durationToSpanInEra = const

-- TODO compare placeAt etc.


-- | Convert an aligned voice to a score.
renderAlignedVoice :: Aligned (Voice a) -> Score a
renderAlignedVoice = renderAligned voiceToScoreInEra

-- | Convert an aligned note to an event.
renderAlignedNote :: Aligned (Note a) -> Event a
renderAlignedNote = renderAligned noteToEventInEra

-- | Convert an aligned duration to a span.
renderAlignedDuration :: Aligned Duration -> Span
renderAlignedDuration = renderAligned durationToSpanInEra





