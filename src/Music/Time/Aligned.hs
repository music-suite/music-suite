{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-deprecations
  -fno-warn-redundant-constraints #-}

module Music.Time.Aligned
  ( -- * Aligned type
    Aligned,

    -- ** Creating aligned values
    aligned,
    alignTo,
    (||>),

    -- ** Update aligned values
    align,
    realign,

    -- ** Extracting/rendering aligned values
    renderAligned,
    renderAlignedVoice,
    renderAlignedNote,
    renderAlignedDuration,

    -- * Conversion functions
    voiceAtDuration,
    voiceToBehavior,
    scoreToBehavior,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Monoid
import Music.Dynamics.Literal
import Music.Pitch.Literal
import Music.Time.Behavior
import Music.Time.Event
import Music.Time.Internal.Preliminaries
import Music.Time.Juxtapose
import Music.Time.Note
import Music.Time.Score
import Music.Time.Voice

-- | Update the local origin of a value, but not its attachment point.
--
--   If the new alignment is different from the previous alignment,
--   the value is moved.
--
-- @
-- align x (aligned t y) = aligned t x
-- align x (align y v) = align x v
-- @
align :: Alignment -> Aligned a -> Aligned a
align l (Aligned ((t, _), a)) = Aligned ((t, l), a)

-- type AlignedVoice a = Aligned (Voice a)

-- | 'Aligned' places a vector-like object in space, by fixing a local duration interpolating
-- the vector to a specific point in time. The aligned value must be an instance of
-- 'HasDuration', with @'view' 'duration'@ providing the size of the vector.
--
-- This is analogous to alignment in a graphical program. To align something at onset, midpoint
-- or offset, use 0, 0.5 or 1 as the local duration value.
newtype Aligned v = Aligned {_getAligned :: ((Time, Alignment), v)}
  deriving (Functor, Eq, Ord, Foldable, Traversable)

-- | Align so that the given part of the value occurs at the given time.
--
-- @
-- aligned t 0 = onsetAt t
-- aligned t 1 = offsetAt t
-- @
aligned :: Time -> Alignment -> v -> Aligned v
aligned t d a = Aligned ((t, d), a)

-- | Align so that the given local duration occurs at the given time.
--
-- If @_duration x = 1@ then @aligned t d x = alignTo t d x@
alignTo :: (Transformable a, HasDuration a) => Time -> Duration -> a -> Aligned a
alignTo t d x = aligned t (d / view duration x) x

-- | Upbeat composition.
--
-- @a ||> b@ is the same as @a <> b@ aligned to the offset of @b@ (and therefore also onset of @b@).
(||>) :: (Transformable a, HasDuration a, Semigroup a) => a -> a -> Aligned a
a ||> b = alignTo 0 (view duration a) (a <> b)

instance Show a => Show (Aligned a) where
  show (Aligned ((t, d), v)) = "aligned (" ++ show t ++ ") (" ++ show d ++ ") (" ++ show v ++ ")"

instance ToJSON a => ToJSON (Aligned a) where
  toJSON (Aligned ((t, d), v)) = JSON.object [("alignment", toJSON d), ("origin", toJSON t), ("value", toJSON v)]

instance Transformable v => Transformable (Aligned v) where
  transform s (Aligned ((t, d), v)) = Aligned ((transform s t, d), transform s v)

instance (HasDuration v, Transformable v) => HasDuration (Aligned v) where
  _duration (Aligned (_, v)) = v ^. duration

instance (HasDuration v, Transformable v) => HasPosition1 (Aligned v) where
  -- _position (Aligned (position, alignment, v)) = alerp (position .-^ (size * alignment)) (position .+^ (size * (1-alignment)))
  _era1 (Aligned ((p, a), v)) =
    (p .-^ (size * a)) <-> (p .+^ (size * (1 - a)))
    where
      size = v ^. duration

instance (HasDuration v, Transformable v) => HasPosition (Aligned v) where
  _era = Just . _era1

-- |  Change the alignment of a value without moving it.
--
--  @
--  x^.'era' = ('realign' l x)^.'era'
--  @
realign :: (HasDuration a, Transformable a) => Alignment -> Aligned a -> Aligned a
realign l a@(Aligned ((_t, _), x)) = Aligned ((a ^. position l, l), x)

-- | Render an aligned value. The given span represents the actual span of the aligned value.
renderAligned :: (HasDuration a, Transformable a) => (Span -> a -> b) -> Aligned a -> b
renderAligned f a@(Aligned (_, v)) = f (_era1 a) v

-- Somewhat suspect, see below for clarity...

voiceToScoreInEra :: Span -> Voice a -> Score a
voiceToScoreInEra e = setEra e . pseq . map (uncurry stretch) . view pairs . fmap pure

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

-- |
-- Treat a voice as a partial function of time.
--
-- >>> voiceAtDuration (stretch 2 c <> stretch 3 d) (-1)
-- Nothing
--
-- >>> voiceAtDuration (stretch 2 c <> stretch 3 d)) 0
-- Just c
--
-- >>> voiceAtDuration (stretch 2 c <> stretch 3 d) 2
-- Just d
--
-- >>> voiceAtDuration (stretch 2 c <> stretch 3 d) 6
-- Nothing
voiceAtDuration :: Voice a -> Duration -> Maybe a
voiceAtDuration v =
  Data.Monoid.getLast . voiceAtDuration' (fmap pure v)

voiceAtDuration' :: Monoid a => Voice a -> Duration -> a
voiceAtDuration' v d = voiceToBehavior (aligned o 0 v) ! (o .+^ d)
  where
    o = 0 -- Any value will do

-- Turn a voice into a behavior by aligning.
-- Value at switchpoint is determined by the monoid (see voiceAtDurationFirst etc).
voiceToBehavior :: Monoid a => Aligned (Voice a) -> Behavior a
voiceToBehavior = scoreToBehavior . renderAlignedVoice

scoreToBehavior :: Monoid a => Score a -> Behavior a
scoreToBehavior = concatB . fmap pure
