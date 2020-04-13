{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME TODO get rid of!
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- FIXME

{-
 - TODO get rid of UndecidableInstances by tracking both pitch and interval in the type
 - params of Mode/Scale etc.
 -}

-- | Scales and chords.
--
-- Semantically is little distinction between a Scale and a Chord. Thus the 'Chord' and 'Scale'
-- types are synonyms. We use a newtype wrapper to get a more idiomatic 'Inspectable' instance.
--
-- Semantically @Chord p@ and @Scale p@ are countable subsets of some pitch space p.
--
-- A 'Mode' (or 'ChordType) is like a Chord/Scale that has forgotten its origin.
module Music.Pitch.Scale
{-
  ( -- * TODO NEW
    Orientation(..),
    Rooting(..),
    ScaleChord(..),
    MODE,
    SCALE,
    CHORD,
    CHORDTYPE,

    -- * TODO move
    Countable (..),
    Generated (..),

    -- * Modes and Chord types
    Mode (..),
    modeFromSteps,
    modeIntervals,
    ChordType,
    functionFromSteps,
    functionIntervals,

    -- * Scales
    Scale,
    scaleTonic,
    scaleMode,
    leadingInterval,
    invertMode,
    scale,
    scaleToList,

    -- * Chords
    Chord,
    chordTonic,
    chordType,
    complementInterval,
    invertChord,
    chord,
    chordToList,
    repeatingInterval,

    -- * Common modes, scales and chords

    -- ** Major-Minor/Common practice modes
    majorScale,
    pureMinorScale,
    harmonicMinorScale,
    melodicMinorScaleUp,

    -- ** Church/Gregorian modes
    aeolian,
    locrian,
    ionian,
    dorian,
    phrygian,
    lydian,
    mixolydian,

    -- ** Other modes
    majorPentaTonic,
    minorPentaTonic,
    bluesMinor,
    bluesMajor,
    bebopScale,
    wholeTone,
    octatonic,

    -- ** Modes of limited transposition
    firstMode,
    secondMode,
    thirdMode,
    fourthMode,
    fifthMode,
    sixthMode,
    seventhMode,

    -- ** Triadic/Common practice chord types
    majorTriad,
    minorTriad,
    augmentedChord,
    diminishedChord,
    halfDiminishedChord,
    majorMinorSeventhChord,
    majorMajorSeventhChord,
    minorMinorSeventhChord,
    minorMajorSeventhChord,
    frenchSixthChord,
    germanSixthChord,

    -- ** Non-triadic chord types
    repeating,
    chromaticCluster,
    wholeToneCluster,
    quartal,
    quintal,

    -- * Voiced chords/scales
    Voiced (..),
    getVoiced,
    voiced,
    voiceIn,
    invertVoicing,
  )
-}
where

import Control.Lens (Lens, Lens', coerced)
import Data.AffineSpace
import Data.AffineSpace.Point.Offsets
  ( distanceVs,
    offsetPoints,
    offsetPointsS,
    AffinePair,
  )
import Data.Foldable
import Data.Kind
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream.Infinite (Stream)
import qualified Data.Stream.Infinite as Stream
import Data.VectorSpace
import Music.Pitch.Common hiding (Mode)
import Music.Pitch.Literal
import Music.Pitch.Literal
import Music.Score.Pitch (HasPitches (..))
import qualified Music.Score.Pitch as S

data Orientation = Seq | Par
data Rooting     = NoRoot | Root

data ScaleChord :: Orientation -> Rooting -> Type -> Type -> Type where
  Mode :: NonEmpty v -> ScaleChord a 'NoRoot v p
  ScaleChord ::
    p
    -> ScaleChord o 'NoRoot v p
    -> ScaleChord o 'Root   v p

type Mode       = ScaleChord 'Seq 'NoRoot
type Scale      = ScaleChord 'Seq 'Root
type ChordType  = ScaleChord 'Par 'NoRoot
type Chord      = ScaleChord 'Par 'Root

deriving instance (Eq v, Eq p) => Eq (ScaleChord o r v p)
deriving instance (Ord v, Ord p) => Ord (ScaleChord o r v p)
deriving instance (Show v, Show p) => Show (ScaleChord o r v p)
deriving instance Functor (ScaleChord o r v)
deriving instance Foldable (ScaleChord o r v)
deriving instance Traversable (ScaleChord o r v)

instance Bifunctor (ScaleChord o r) where
  bimap = bimapDefault

instance Bifoldable (ScaleChord o r) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (ScaleChord o r) where
  bitraverse f _ (Mode xs) = Mode <$> traverse f xs
  bitraverse f g (ScaleChord p xs) = ScaleChord <$> g p <*> bitraverse f g xs


type instance S.Pitch (ScaleChord o r v p) = S.Pitch p

type instance S.SetPitch p (ScaleChord o r v _p) = ScaleChord o r v (S.SetPitch p _p)

instance HasPitches p1 p2 => HasPitches (ScaleChord o r v p1) (ScaleChord o r v p2) where
  pitches = traverse . pitches

-- |
-- @
-- majorScale = modeFromSteps [M2,M2,m3,M2,M2,M2,m3]
--
-- > [Interval] -> Mode Pitch
modeFromSteps :: NonEmpty v -> Mode v p
modeFromSteps = Mode

-- |
-- > Lens' (Mode Pitch) [Interval]
modeIntervals :: Lens' (Mode v p) (NonEmpty v)
modeIntervals f (Mode is) = fmap (\is -> Mode is) $ f is


scale :: AffinePair v p => p -> Mode v p -> Scale v p
scale = ScaleChord

-- |
-- > Lens' (Scale Pitch) Pitch
scaleTonic :: Lens' (Scale v p) p
scaleTonic f (ScaleChord t xs) = fmap (\t -> ScaleChord t xs) $ f t

-- |
-- > Lens' (Scale Pitch) (Mode Pitch)
scaleMode :: Lens' (Scale v p) (Mode v p)
scaleMode f (ScaleChord t xs) = fmap (\xs -> ScaleChord t xs) $ f xs

-- |
--
-- >>> repeatingInterval majorScale
-- _P8
repeatingInterval :: AffinePair v p => Mode v p -> v
repeatingInterval (Mode xs) = sumV xs

-- |
--
-- >>> leadingInterval majorScale
-- m2
-- >>> leadingInterval harmonicMinorScale
-- m2
-- >>> leadingInterval pureMinorScale
-- _M2
leadingInterval :: AffinePair v p => Mode v p -> v
leadingInterval (Mode xs) = NonEmpty.last xs

isHeadOf :: Eq a => a -> Stream a -> Bool
isHeadOf a (b Stream.:> _) = a == b

-- |
-- @
-- length (generator x) == n -> invertMode n x = x
-- @
invertMode :: AffinePair v p => Integer -> Mode v p -> Mode v p
invertMode n (Mode xs) = Mode (rotate n xs)


-- TODO move

-- |
-- Class of types representing finite sets.
--
-- @
-- n `mod` length n == 0 -> rotate n = id
-- length (rotate n xs) = length xs
-- rotate n = rotate n . toList
-- @
class Foldable f => FiniteSequence f where
  rotate :: Integer -> f a -> f a

instance FiniteSequence [] where
  rotate n xs = take lxs . drop (fromInteger n `mod` lxs) . cycle $ xs
    where
      lxs = length xs

-- TODO optimize
instance FiniteSequence NonEmpty where
  rotate :: Integer -> NonEmpty a -> NonEmpty a
  rotate n xs
    | n > 0 =
      NonEmpty.iterate left xs NonEmpty.!! fromInteger n
    | n == 0 = xs
    | n < 0 =
      NonEmpty.iterate right xs NonEmpty.!! (negate $ fromInteger n)
    | otherwise = error "impossible"
    where
      left (x :| []) = x :| []
      left (x :| y : rs) = y :| (rs ++ [x])
      right = NonEmpty.reverse . left . NonEmpty.reverse


-- TODO semantically suspect!
scaleToList :: AffinePair v p => Scale v p -> [p]
scaleToList (ScaleChord tonic (Mode leaps)) = init $ offsetPoints tonic $ toList leaps


-- |
-- Returns the interval sequenc that generates the given mode.
--
-- The length of this sequence gives the number of notes in the scale,
-- e.g. for a pentatonic (5-note) scale @s@, @length (generator s) == 5@,
-- for a heptatonic (7-note) scale it is 7, and so on.
generator :: ScaleChord a 'Root v p -> NonEmpty v
generator (ScaleChord _ (Mode x)) = x

{-

class Countable f where

  -- | Convert to a countably infinite set (represented as a tuple
  -- of "negative", "zero" and "positive" components.
  scaleToSet :: AffineSpace a => f a -> (Stream a, a, Stream a)

  index :: AffineSpace p => f p -> Integer -> p

  member :: (Ord p, AffineSpace p) => f p -> p -> Bool

  index s n = case fromIntegral n of
    n
      | n > 0 -> pos Stream.!! (n - 1)
      | n == 0 -> z
      | n < 0 -> neg Stream.!! negate (n + 1)
      | otherwise -> error "impossible"
    where
      (neg, z, pos) = scaleToSet s

  member s p = case p of
    p
      | p > z -> p `isHeadOf` Stream.dropWhile (< p) pos
      | p == z -> True
      | p < z -> p `isHeadOf` Stream.dropWhile (> p) neg
      | otherwise -> error "impossible"
    where
      (neg, z, pos) = scaleToSet s

instance Countable Scale where
  scaleToSet :: AffineSpace a => Scale a -> (Stream a, a, Stream a)
  scaleToSet (Scale tonic (Mode leaps)) =
    ( Stream.tail $ offsetPointsS tonic $ fmap negateV $ Stream.cycle $ NonEmpty.reverse leaps,
      tonic,
      Stream.tail $ offsetPointsS tonic $ Stream.cycle leaps
    )

instance Countable Chord where
  scaleToSet = scaleToSet . getChord

-- TODO rename to ChordType or similar
type ChordType a = Mode a

-- |
-- > [Interval] -> Interval -> ChordType Pitch
functionFromSteps :: NonEmpty (Diff a) -> ChordType a
functionFromSteps = modeFromSteps

-- |
-- > Lens' (ChordType Pitch) [Interval]
functionIntervals :: Lens' (ChordType a) (NonEmpty (Diff a))
functionIntervals = modeIntervals

-- Note: The only difference between a chord and a Scale is the Inspectable instance
newtype Chord a = Chord {getChord :: Scale a}

deriving instance (Eq a, Eq (Diff a)) => Eq (Chord a)

deriving instance (Ord a, Ord (Diff a)) => Ord (Chord a)

deriving instance (Show a, Show (Diff a)) => Show (Chord a)

chord :: AffineSpace a => a -> ChordType a -> Chord a
chord x xs = Chord $ scale x xs

-- |
-- > Lens' (Chord Pitch) Pitch
chordTonic :: Lens' (Chord a) a
chordTonic = coerced . scaleTonic

-- |
-- > Lens' (Chord Pitch) (ChordType Pitch)
chordType :: Lens' (Chord a) (ChordType a)
chordType = coerced . scaleMode

-- |
--
-- >>> complementInterval majorTriad
-- _P4
-- >>> complementInterval minorTriad
-- _P4
-- >>> complementInterval majorMinorSeventhChord
-- _M2
--
-- > Lens' (ChordType Pitch) Interval
complementInterval :: AffineSpace a => ChordType a -> Diff a
complementInterval = leadingInterval

invertChord :: AffineSpace a => Integer -> ChordType a -> ChordType a
invertChord = invertMode


-- | Returns a single inversion of the given chord (no repeats!).
chordToList :: AffineSpace a => Chord a -> [a]
chordToList = scaleToList . getChord

-- Common scales

majorScale :: Mode Pitch
majorScale = Mode [_M2, _M2, m2, _M2, _M2, _M2, m2]

-- |
-- @
-- pureMinorScale = invertMode 5 majorScale
-- majorScale     = invertMode 2 pureMinorScale
-- @
pureMinorScale :: Mode Pitch
pureMinorScale = Mode [_M2, m2, _M2, _M2, m2, _M2, _M2]

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = Mode [_M2, m2, _M2, _M2, m2, _A2, m2]

melodicMinorScaleUp :: Mode Pitch
melodicMinorScaleUp = Mode [_M2, m2, _M2, _M2, _M2, _M2, m2]

ionian :: Mode Pitch
ionian = invertMode 0 majorScale

dorian :: Mode Pitch
dorian = invertMode 1 majorScale

phrygian :: Mode Pitch
phrygian = invertMode 2 majorScale

lydian :: Mode Pitch
lydian = invertMode 3 majorScale

mixolydian :: Mode Pitch
mixolydian = invertMode 4 majorScale

aeolian :: Mode Pitch
aeolian = invertMode 5 majorScale

locrian :: Mode Pitch
locrian = invertMode 6 majorScale

majorPentaTonic :: Mode Pitch
majorPentaTonic = Mode [_M2, _M2, m3, _M2, m3]

bluesMinor :: Mode Pitch
bluesMinor = invertMode 2 majorPentaTonic

bluesMajor :: Mode Pitch
bluesMajor = invertMode 3 majorPentaTonic

minorPentaTonic :: Mode Pitch
minorPentaTonic = invertMode 4 majorPentaTonic

-- a.k.a. "bebop major"
bebopScale :: Mode Pitch
bebopScale = Mode [_M2, _M2, m2, _M2, m2, m2, _M2, m2]

wholeTone :: Mode Pitch
wholeTone = firstMode

octatonic :: Mode Pitch
octatonic = secondMode

firstMode :: Mode Pitch
firstMode = Mode [_M2, _M2, _M2, _M2, _M2, _M2]

secondMode :: Mode Pitch
secondMode = Mode [m2, _M2, _A1, _M2, m2, _M2, m2, _M2]

thirdMode :: Mode Pitch
thirdMode = Mode [_M2, m2, m2, _M2, m2, m2, _M2, m2, m2]

fourthMode :: Mode Pitch
fourthMode = Mode [m2, m2, m3, m2, m2, m2, m3, m2]

fifthMode :: Mode Pitch
fifthMode = Mode [m2, _M3, m2, m2, _M3, m2]

sixthMode :: Mode Pitch
sixthMode = Mode [_M2, _M2, m2, m2, _M2, _M2, m2, m2]

seventhMode :: Mode Pitch
seventhMode = Mode [m2, m2, m2, _M2, m2, m2, m2, m2, _M2, m2]

-- Common chords

majorTriad :: ChordType Pitch
majorTriad = Mode [_M3, m3, _P4]

minorTriad :: ChordType Pitch
minorTriad = Mode [m3, _M3, _P4]

augmentedChord :: ChordType Pitch
augmentedChord = Mode [_M3, _M3, _M3]

diminishedChord :: ChordType Pitch
diminishedChord = Mode [m3, m3, m3, m3]

halfDiminishedChord :: ChordType Pitch
halfDiminishedChord = Mode [m3, m3, _M3, _M2]

-- | Also known as "dominant seventh".
majorMinorSeventhChord :: ChordType Pitch
majorMinorSeventhChord = Mode [_M3, m3, m3, _M2]

-- | Also known as "major seventh".
majorMajorSeventhChord :: ChordType Pitch
majorMajorSeventhChord = Mode [_M3, m3, _M3, m2]

-- | Also known as a "ii7". First inversion of major sixth.
minorMinorSeventhChord :: ChordType Pitch
minorMinorSeventhChord = Mode [m3, _M3, m3, _M2]

-- | Also known as "major seventh".
minorMajorSeventhChord :: ChordType Pitch
minorMajorSeventhChord = Mode [m3, _M3, _M3, m2]

-- TODO ninth chords, sixths chords, 6/9 (pentatonic field), Guido's hexachord?

germanSixthChord :: ChordType Pitch
germanSixthChord = majorMinorSeventhChord

frenchSixthChord :: ChordType Pitch
frenchSixthChord = Mode [_M3, d3, _M3, _M2]

-- | Build a harmonic filed from repeating a single interval.
--
-- Up to enharmonic equivalence, we have:
--
-- @
-- repeating _m2 = chromaticCluster
-- repeating M2  = wholeToneCluster
-- repeating m3  = diminishedChord
-- repeating P4  = quartal
-- @
repeating :: Diff p -> ChordType p
repeating x = Mode (x NonEmpty.:| [])

chromaticCluster :: ChordType Pitch
chromaticCluster = repeating m2

wholeToneCluster :: ChordType Pitch
wholeToneCluster = repeating _M2

quartal :: ChordType Pitch
quartal = repeating _P4

quintal :: ChordType Pitch
quintal = repeating _P5

data Voiced f p = Voiced {getChordScale :: f p, getSteps :: NonEmpty Integer}

deriving instance Eq (f a) => Eq (Voiced f a)

deriving instance Ord (f a) => Ord (Voiced f a)

deriving instance Show (f a) => Show (Voiced f a)

getVoiced :: (AffineSpace p, Countable f) => Voiced f p -> NonEmpty p
getVoiced x = index (getChordScale x) <$> getSteps x

voiced :: Generated f => f p -> Voiced f p
voiced x = voiceIn (fromIntegral $ length (generator x)) x

-- | Like 'voiced' but include the leading/repeating interval.
voicedLong :: Generated f => f p -> Voiced f p
voicedLong x = voiceIn (fromIntegral $ length (generator x) + 1) x

voiceIn :: Integer -> f p -> Voiced f p
voiceIn n x = Voiced x [0 .. n - 1]

invertVoicing :: Integer -> Voiced f a -> Voiced f a
invertVoicing n (Voiced f xs) = Voiced f (fmap (+ n) xs)
-}
