{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Scales and chords.
--
module Music.Pitch.Scale
  (
    -- * Basic types
    Orientation (..),
    Rooting (..),
    ScaleChord (..),

    -- * Scales
    Scale,
    scale,

    -- * Chords
    Chord,
    chord,
    invertChord,
    repeatingInterval,

    -- * Scales and chords as sets
    -- ** Generator
    generator,
    -- ** Indexing
    root,
    index,
    member,
    tabulate,

    -- * Modes
    Mode,
    leadingInterval,
    invertMode,
    -- ** Common
    -- *** Major-Minor/Common practice
    majorScale,
    pureMinorScale,
    harmonicMinorScale,
    melodicMinorScaleUp,
    -- *** Church modes
    aeolian,
    locrian,
    ionian,
    dorian,
    phrygian,
    lydian,
    mixolydian,
    -- *** Other modes
    majorPentaTonic,
    minorPentaTonic,
    bluesMinor,
    bluesMajor,
    bebopScale,
    wholeTone,
    octatonic,

    -- * Chord types
    ChordType,
    complementInterval,
    -- ** Common
    -- *** Major-Minor/Common practice
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
    -- *** Clusters
    repeating,
    chromaticCluster,
    wholeToneCluster,
    quartal,
    quintal,
    -- *** Limited transposition
    firstMode,
    secondMode,
    thirdMode,
    fourthMode,
    fifthMode,
    sixthMode,
    seventhMode,


    -- * Conversions
    chordToScale,
    scaleToChord,
    chordTypeToMode,
    modeToChordType,
    reorient,
    chordToList,
    scaleToList,




    -- * Voicing
    Voiced (..),
    getVoiced,
    voiced,
    voiceIn,
    invertVoicing,
  )
where

import Control.Lens (Lens')
import Data.AffineSpace.Point.Offsets
  ( AffinePair,
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
import Music.Score.Pitch (HasPitches (..))
import qualified Music.Score.Pitch as S

-- | This is used to distinguish between a 'Scale' and a 'Chord', and between a 'Mode' and 'ChordType'.
data Orientation = Seq | Par

-- | This is used to distinguish between a 'Scale' and a 'Mode', and between a 'Chord' and a 'ChordType'.
data Rooting = NoRoot | Root

-- | Represents a repeating set of notes. It has for parameters:
--
-- * 'Orientation'
-- * 'Rooting'
-- * Pitch type (usually 'Pitch')
-- * Interval type (usually 'Interval')
--
-- You may want work with a more specific type such as 'Scale' or 'Chord'.
--
data ScaleChord :: Orientation -> Rooting -> Type -> Type -> Type where
  Mode :: NonEmpty v -> ScaleChord a 'NoRoot v p
  ScaleChord ::
    p ->
    ScaleChord o 'NoRoot v p ->
    ScaleChord o 'Root v p

-- | A repeating set of notes without a root.
--
-- Examples:
--
-- * 'dorian',
-- * 'majorScale',
-- * 'melodicMinorScaleUp'
type Mode = ScaleChord 'Seq 'NoRoot

-- | A repeating set of notes with a root.
--
-- Examples:
--
-- * 'scale' 'd' 'dorian'
type Scale = ScaleChord 'Seq 'Root

type ChordType = ScaleChord 'Par 'NoRoot

-- | A repeating set of notes without a root.
--
-- This represents a chord in the abstract sense without a specific voicing.
-- For voiced chords, see 'Voiced'.
--
-- Example:
--
-- * 'chord' 'g' 'dominantSeventhChord'
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

type instance S.GetPitch (ScaleChord o r v p) = S.GetPitch p

type instance S.SetPitch p (ScaleChord o r v p') = ScaleChord o r v (S.SetPitch p p')

instance HasPitches p p' => HasPitches (ScaleChord o r v p) (ScaleChord o r v p') where
  pitches = traverse . pitches

mode :: NonEmpty v -> Mode v p
mode = Mode

modeIntervals :: Lens' (Mode v p) (NonEmpty v)
modeIntervals f (Mode is) = fmap (\is -> Mode is) $ f is

-- | Build a scale from a root and mode.
--
-- >>> scale c majorScale
-- ScaleChord c (Mode (_M2 :| [_M2,m2,_M2,_M2,_M2,m2]))
--
-- >>> scale f lydian
-- ScaleChord f (Mode (_M2 :| [_M2,_M2,m2,_M2,_M2,m2]))
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

-- TODO move FiniteSequence

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
      NonEmpty.iterate right xs NonEmpty.!! negate (fromInteger n)
    | otherwise = error "impossible"
    where
      left (x :| []) = x :| []
      left (x :| y : rs) = y :| (rs ++ [x])
      right = NonEmpty.reverse . left . NonEmpty.reverse

-- TODO semantically suspect!
scaleToList :: AffinePair v p => Scale v p -> [p]
scaleToList (ScaleChord tonic (Mode leaps)) = init $ offsetPoints tonic $ toList leaps

-- |
-- Returns the interval sequence that generates the given 'ScaleChord'.
--
-- The length of this sequence gives the number of notes in the scale.  For
-- example a pentatonic scale @s@ has a generator of length 5.
generator :: ScaleChord o r v p -> NonEmpty v
generator (Mode x) = x
generator (ScaleChord _ xs) = generator xs
{-# INLINEABLE generator #-}

-- |
-- Returns the root of a 'Scale' or 'Chord'.
root :: ScaleChord o 'Root v p -> p
root (ScaleChord r _) = r
{-# INLINEABLE root #-}

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

-- | View a 'Scale' or a 'Chord' as a countably infinite set of pitches.
--
-- You can use this to enumerate a scale upwards , starting
-- from the root.
--
-- @
-- fmap (scale d dorian `index`) [0..]
-- @
--
-- @
-- fmap (scale d dorian `index`) [0,-1,..]
-- @
--
-- @index 0 scale@ returns the root of the scale, for example:
--
-- >>> index (scale c majorScale) 0
-- c
--
-- See also 'tabulate'.
index :: AffinePair v p => ScaleChord o 'Root v p -> Integer -> p
index s n = case fromIntegral n of
  n
    | n > 0 -> pos Stream.!! (n - 1)
    | n == 0 -> z
    | n < 0 -> neg Stream.!! negate (n + 1)
    | otherwise -> error "impossible"
  where
    (neg, z, pos) = tabulate s

-- | Check if a pitch is a member of the given 'Scale' or 'Chord'.
--
-- >>> c'' `member` scale d dorian
-- True
--
-- >>> cs'' `member` scale d dorian
-- False
member :: (Ord p, AffinePair v p) => p -> ScaleChord o 'Root v p -> Bool
member p s = case p of
  p
    | p > z -> p `isHeadOf` Stream.dropWhile (< p) pos
    | p == z -> True
    | p < z -> p `isHeadOf` Stream.dropWhile (> p) neg
    | otherwise -> error "impossible"
  where
    (neg, z, pos) = tabulate s

-- | Returns the root of a 'Scale' or 'Chord', along with the infinite
-- streams of pitches downward and upward.
--
-- See also 'index'.
tabulate :: AffinePair v p => ScaleChord o 'Root v p -> (Stream p, p, Stream p)
tabulate (ScaleChord tonic (Mode leaps)) =
  ( Stream.tail $ offsetPointsS tonic $ fmap negateV $ Stream.cycle $ NonEmpty.reverse leaps,
    tonic,
    Stream.tail $ offsetPointsS tonic $ Stream.cycle leaps
  )

-- | Build a chord from a root and a chord type.
--
-- >>> chord c minorTriad
-- ScaleChord c (Mode (m3 :| [_M3,_P4]))
--
-- >>> chord c dominantSeventhChord
-- ScaleChord c (Mode (_M3 :| [m3,m3,_M2]))
--
-- >>> chord d halfDiminishedChord
-- ScaleChord d (Mode (m3 :| [m3,_M3,_M2]))
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

-- TODO semantically suspect!
chordToList :: AffinePair v p => Chord v p -> [p]
chordToList (ScaleChord tonic (Mode leaps)) = init $ offsetPoints tonic $ toList leaps


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

instance Bifunctor f => Bifunctor (Voiced f) where
  bimap f g (Voiced x ns) = Voiced (bimap f g x) ns

instance Bifoldable f => Bifoldable (Voiced f) where
  bifoldMap f g (Voiced x _) = bifoldMap f g x

instance Bitraversable f => Bitraversable (Voiced f) where
  bitraverse f g (Voiced xs ns) = Voiced <$> bitraverse f g xs <*> pure ns

type instance S.GetPitch (Voiced f v p) = S.GetPitch p

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
