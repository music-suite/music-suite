{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- FIXME TODO get rid of!

-- FIXME

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
  ( -- * TODO NEW
    Orientation (..),
    Rooting (..),
    ScaleChord (..),
    generator,
    index,
    member,
    tabulate,

    -- * Modes and Chord types
    Mode,
    ChordType,
    -- modeIntervals,
    -- chordTypeIntervals,
    leadingInterval,
    complementInterval,
    invertMode,

    -- * Scales
    Scale,
    scale,
    scaleToList,
    -- scaleTonic,
    -- scaleMode,

    -- * Chords
    Chord,
    chord,
    chordToList,
    -- chordTonic,
    -- chordType,
    invertChord,
    repeatingInterval,

    -- * Conversions
    chordToScale,
    scaleToChord,
    chordTypeToMode,
    modeToChordType,
    reorient,

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
    dominantSeventhChord,
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
where

import Control.Lens (Lens, Lens', coerced)
import Data.AffineSpace
import Data.AffineSpace.Point.Offsets
  ( AffinePair,
    distanceVs,
    offsetPoints,
    offsetPointsS,
  )
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Kind
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream.Infinite (Stream)
import qualified Data.Stream.Infinite as Stream
import Data.VectorSpace
import Music.Pitch.Common
import Music.Pitch.Literal
import Music.Pitch.Literal
import Music.Score.Pitch (HasPitches (..))
import qualified Music.Score.Pitch as S

data Orientation = Seq | Par

data Rooting = NoRoot | Root

data ScaleChord :: Orientation -> Rooting -> Type -> Type -> Type where
  Mode :: NonEmpty v -> ScaleChord a 'NoRoot v p
  ScaleChord ::
    p ->
    ScaleChord o 'NoRoot v p ->
    ScaleChord o 'Root v p

type Mode = ScaleChord 'Seq 'NoRoot

type Scale = ScaleChord 'Seq 'Root

type ChordType = ScaleChord 'Par 'NoRoot

type Chord = ScaleChord 'Par 'Root

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

type instance S.SetPitch p (ScaleChord o r v p') = ScaleChord o r v (S.SetPitch p p')

instance HasPitches p p' => HasPitches (ScaleChord o r v p) (ScaleChord o r v p') where
  pitches = traverse . pitches

mode :: NonEmpty v -> Mode v p
mode = Mode

modeIntervals :: Lens' (Mode v p) (NonEmpty v)
modeIntervals f (Mode is) = fmap (\is -> Mode is) $ f is

scale :: AffinePair v p => p -> Mode v p -> Scale v p
scale = ScaleChord

scaleTonic :: Lens' (Scale v p) p
scaleTonic f (ScaleChord t xs) = fmap (\t -> ScaleChord t xs) $ f t

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
-- Class of types representing finite sequences.
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
generator :: ScaleChord o r v p -> NonEmpty v
generator (Mode x) = x
generator (ScaleChord _ (Mode x)) = x

reorient :: ScaleChord o r v p -> ScaleChord o' r v p
reorient (Mode xs) = Mode xs
reorient (ScaleChord x xs) = ScaleChord x (reorient xs)

chordTypeToMode :: ChordType v p -> Mode v p
chordTypeToMode = reorient

modeToChordType :: Mode v p -> ChordType v p
modeToChordType = reorient

scaleToChord :: Scale v p -> Chord v p
scaleToChord = reorient

chordToScale :: Chord v p -> Scale v p
chordToScale = reorient

index :: AffinePair v p => ScaleChord o 'Root v p -> Integer -> p
index s n = case fromIntegral n of
  n
    | n > 0 -> pos Stream.!! (n - 1)
    | n == 0 -> z
    | n < 0 -> neg Stream.!! negate (n + 1)
    | otherwise -> error "impossible"
  where
    (neg, z, pos) = tabulate s

member :: (Ord p, AffinePair v p) => ScaleChord o 'Root v p -> p -> Bool
member s p = case p of
  p
    | p > z -> p `isHeadOf` Stream.dropWhile (< p) pos
    | p == z -> True
    | p < z -> p `isHeadOf` Stream.dropWhile (> p) neg
    | otherwise -> error "impossible"
  where
    (neg, z, pos) = tabulate s

-- Could be generalized to ScaleChord o Root (TODO flip type params!)
tabulate :: AffinePair v p => ScaleChord o 'Root v p -> (Stream p, p, Stream p)
tabulate (ScaleChord tonic (Mode leaps)) =
  ( Stream.tail $ offsetPointsS tonic $ fmap negateV $ Stream.cycle $ NonEmpty.reverse leaps,
    tonic,
    Stream.tail $ offsetPointsS tonic $ Stream.cycle leaps
  )

chord :: AffinePair v p => p -> ChordType v p -> Chord v p
chord = ScaleChord

chordTypeIntervals :: Lens' (ChordType v p) (NonEmpty v)
chordTypeIntervals f (Mode is) = fmap (\is -> Mode is) $ f is

chordTonic :: Lens' (Chord v p) p
chordTonic f (ScaleChord t xs) = fmap (\t -> ScaleChord t xs) $ f t

chordType :: Lens' (Chord v p) (ChordType v p)
chordType f (ScaleChord t xs) = fmap (\xs -> ScaleChord t xs) $ f xs

-- |
--
-- >>> complementInterval majorTriad
-- _P4
-- >>> complementInterval minorTriad
-- _P4
-- >>> complementInterval majorMinorSeventhChord
-- _M2
complementInterval :: AffinePair v p => ChordType v p -> v
complementInterval (Mode xs) = NonEmpty.last xs

invertChord :: AffinePair v p => Integer -> ChordType v p -> ChordType v p
invertChord n (Mode xs) = Mode (rotate n xs)

-- | Returns a single inversion of the given chord (no repeats!).
chordToList :: AffinePair v p => Chord v p -> [p]
chordToList (ScaleChord tonic (Mode leaps)) = init $ offsetPoints tonic $ toList leaps

-- Common scales

majorScale :: Mode Interval Pitch
majorScale = Mode [_M2, _M2, m2, _M2, _M2, _M2, m2]

-- |
-- @
-- pureMinorScale = invertMode 5 majorScale
-- majorScale     = invertMode 2 pureMinorScale
-- @
pureMinorScale :: Mode Interval Pitch
pureMinorScale = Mode [_M2, m2, _M2, _M2, m2, _M2, _M2]

harmonicMinorScale :: Mode Interval Pitch
harmonicMinorScale = Mode [_M2, m2, _M2, _M2, m2, _A2, m2]

melodicMinorScaleUp :: Mode Interval Pitch
melodicMinorScaleUp = Mode [_M2, m2, _M2, _M2, _M2, _M2, m2]

ionian :: Mode Interval Pitch
ionian = invertMode 0 majorScale

dorian :: Mode Interval Pitch
dorian = invertMode 1 majorScale

phrygian :: Mode Interval Pitch
phrygian = invertMode 2 majorScale

lydian :: Mode Interval Pitch
lydian = invertMode 3 majorScale

mixolydian :: Mode Interval Pitch
mixolydian = invertMode 4 majorScale

aeolian :: Mode Interval Pitch
aeolian = invertMode 5 majorScale

locrian :: Mode Interval Pitch
locrian = invertMode 6 majorScale

majorPentaTonic :: Mode Interval Pitch
majorPentaTonic = Mode [_M2, _M2, m3, _M2, m3]

bluesMinor :: Mode Interval Pitch
bluesMinor = invertMode 2 majorPentaTonic

bluesMajor :: Mode Interval Pitch
bluesMajor = invertMode 3 majorPentaTonic

minorPentaTonic :: Mode Interval Pitch
minorPentaTonic = invertMode 4 majorPentaTonic

-- | The bebop scale, a.k.a. "bebop major".
bebopScale :: Mode Interval Pitch
bebopScale = Mode [_M2, _M2, m2, _M2, m2, m2, _M2, m2]

wholeTone :: Mode Interval Pitch
wholeTone = firstMode

octatonic :: Mode Interval Pitch
octatonic = secondMode

firstMode :: Mode Interval Pitch
firstMode = Mode [_M2, _M2, _M2, _M2, _M2, _M2]

secondMode :: Mode Interval Pitch
secondMode = Mode [m2, _M2, _A1, _M2, m2, _M2, m2, _M2]

thirdMode :: Mode Interval Pitch
thirdMode = Mode [_M2, m2, m2, _M2, m2, m2, _M2, m2, m2]

fourthMode :: Mode Interval Pitch
fourthMode = Mode [m2, m2, m3, m2, m2, m2, m3, m2]

fifthMode :: Mode Interval Pitch
fifthMode = Mode [m2, _M3, m2, m2, _M3, m2]

sixthMode :: Mode Interval Pitch
sixthMode = Mode [_M2, _M2, m2, m2, _M2, _M2, m2, m2]

seventhMode :: Mode Interval Pitch
seventhMode = Mode [m2, m2, m2, _M2, m2, m2, m2, m2, _M2, m2]

-- Common chords

majorTriad :: ChordType Interval Pitch
majorTriad = Mode [_M3, m3, _P4]

minorTriad :: ChordType Interval Pitch
minorTriad = Mode [m3, _M3, _P4]

augmentedChord :: ChordType Interval Pitch
augmentedChord = Mode [_M3, _M3, _M3]

diminishedChord :: ChordType Interval Pitch
diminishedChord = Mode [m3, m3, m3, m3]

halfDiminishedChord :: ChordType Interval Pitch
halfDiminishedChord = Mode [m3, m3, _M3, _M2]

-- | Also known as "dominant seventh".
majorMinorSeventhChord :: ChordType Interval Pitch
majorMinorSeventhChord = Mode [_M3, m3, m3, _M2]

dominantSeventhChord :: ChordType Interval Pitch
dominantSeventhChord = majorMinorSeventhChord

-- | Also known as "major seventh".
majorMajorSeventhChord :: ChordType Interval Pitch
majorMajorSeventhChord = Mode [_M3, m3, _M3, m2]

-- | Also known as a "ii7". First inversion of major sixth.
minorMinorSeventhChord :: ChordType Interval Pitch
minorMinorSeventhChord = Mode [m3, _M3, m3, _M2]

-- | Also known as "major seventh".
minorMajorSeventhChord :: ChordType Interval Pitch
minorMajorSeventhChord = Mode [m3, _M3, _M3, m2]

-- TODO ninth chords, sixths chords, 6/9 (pentatonic field), Guido's hexachord?

germanSixthChord :: ChordType Interval Pitch
germanSixthChord = majorMinorSeventhChord

frenchSixthChord :: ChordType Interval Pitch
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
repeating :: v -> ChordType v p
repeating x = Mode (x NonEmpty.:| [])

chromaticCluster :: ChordType Interval Pitch
chromaticCluster = repeating m2

wholeToneCluster :: ChordType Interval Pitch
wholeToneCluster = repeating _M2

quartal :: ChordType Interval Pitch
quartal = repeating _P4

quintal :: ChordType Interval Pitch
quintal = repeating _P5

data Voiced f v p = Voiced {getChordScale :: f v p, getSteps :: NonEmpty Integer}

deriving instance (Eq (f v p)) => Eq (Voiced f v p)

deriving instance (Ord (f v p)) => Ord (Voiced f v p)

deriving instance (Show (f v p)) => Show (Voiced f v p)

deriving instance Functor (f v) => Functor (Voiced f v)

deriving instance Foldable (f v) => Foldable (Voiced f v)

deriving instance Traversable (f v) => Traversable (Voiced f v)

-- TODO relax to Bifunctor
instance Bitraversable f => Bifunctor (Voiced f) where
  bimap = bimapDefault

-- TODO relax to Bifoldable
instance Bitraversable f => Bifoldable (Voiced f) where
  bifoldMap = bifoldMapDefault

instance Bitraversable f => Bitraversable (Voiced f) where
  bitraverse f g (Voiced xs ns) = Voiced <$> bitraverse f g xs <*> pure ns

type instance S.Pitch (Voiced f v p) = S.Pitch p

type instance S.SetPitch p (Voiced f v p') = Voiced f v (S.SetPitch p p')

instance
  (Traversable (f v), HasPitches p p') =>
  HasPitches (Voiced f v p) (Voiced f v p')
  where
  pitches = traverse . pitches

-- | Extract the pitches of a voiced chord.
getVoiced :: (AffinePair v p) => Voiced Chord v p -> NonEmpty p
getVoiced x = index (getChordScale x) <$> getSteps x

-- | The default closed voicing.
--
-- @
-- voiced (chord c majorTriad) = [c,e,g]
-- @
voiced :: Chord v p -> Voiced Chord v p
voiced x = voiceIn (fromIntegral $ length (generator x)) x

-- | A closed voicing but including the repeating interval.
--
-- @
-- voiced (chord c majorTriad) = [c,e,g,c']
-- @
voicedLong :: Chord v p -> Voiced Chord v p
voicedLong x = voiceIn (fromIntegral $ length (generator x) + 1) x

-- | A closed voicing with the given number of pitches, using the
-- tonic as root.
--
-- @
-- voicedIn 5 (chord c majorTriad) = [c,e,g,c',e']
-- @
voiceIn :: Integer -> Chord v p -> Voiced Chord v p
voiceIn n x = Voiced x [0 .. n - 1]

-- | Invert a voiced chord.
invertVoicing :: Integer -> Voiced Chord v p -> Voiced Chord v p
invertVoicing n (Voiced f xs) = Voiced f (fmap (+ n) xs)
