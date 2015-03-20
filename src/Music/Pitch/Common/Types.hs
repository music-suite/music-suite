
{-# LANGUAGE FlexibleInstances #-}

-- | Common (Western classical) pitches, intervals and related types.
module Music.Pitch.Common.Types
(
        -- * Even octaves and steps
        Octaves,
        DiatonicSteps,
        ChromaticSteps,
        Semitones,
        
        -- * Intervals
        Number,
        Quality(..),
        QualityType(..),
        IntervalBasis(..),
        Interval(..),

        -- * Pitch
        Name(..),
        Accidental,
        Pitch(..),        
) where

import Data.Typeable

import Music.Pitch.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable

{-|
Number of chromatic steps.
May be negative, indicating a downward interval. 
-}
newtype ChromaticSteps = ChromaticSteps { getChromaticSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

{-| 
Number of diatonic steps.
May be negative, indicating a downward interval. 
-}
newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

{-| 
Number of octaves.
May be negative, indicating a downward interval. 
-}
newtype Octaves = Octaves { getOctaves :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

{-|
Number of semitones.
May be negative, indicating a downward interval. 
-}
type Semitones = ChromaticSteps

{-|
The /number/ component of an interval (fourth, fifth) etc.
May be negative, indicating a downward interval. 
-}
newtype Number = Number { getNumber :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

{-|
The /quality/ component of an interval (minor, major, augmented).
Generalized from single\/double augmented\/diminished to any number of steps.
-}
data Quality
  = Major
  | Minor
  | Perfect
  | Augmented Integer
  | Diminished Integer
  deriving (Eq, Ord, Show)
{-
TODO we really want to use Positive instead of Integer
Alternatively, we could do it as a recursive type

data Quality
  = Major
  | Minor
  | Perfect
  | Augment Quality
  | Diminish Quality
-}

{-|
The alteration implied by a quality is dependent on whether it is attached
to a major\/minor vs. a perfect-style number. This type represents the two possibilities.
-}
data QualityType = PerfectType | MajorMinorType
  deriving (Eq, Ord, Read, Show)

{-|
Accidental, represented as number of alterations.
Generalized from natural and single\/double sharp\/flat to any number of steps.
-}
newtype Accidental = Accidental { getAccidental :: Integer }
  deriving (Eq, Ord, Num, Enum, Real, Integral)

{-|
Pitch name component.
-}
data Name = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum)

{-|
This type represents standard basis for intervbals.
 -}
data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

{-|
Interval type.
 -}
newtype Interval = Interval { getInterval :: (Int, Int) }
  deriving (Eq, Typeable)
    -- Number of (A1,d2) i.e. (chromatic,diatonic) steps

{-|
Pitch type.
-}
newtype Pitch = Pitch { getPitch :: Interval }
  deriving (Eq, Ord, Typeable)



instance Show Octaves where
  show = show . getOctaves

instance Show Number where
  show = show . getNumber

instance Show Accidental where
  show n | n == 0    = "natural"
         | n == 1    = "sharp"
         | n == (-1) = "flat"
         | n == 2    = "doubleSharp"
         | n == (-2) = "doubleFlat"
         | n > 0     = "sharp * " ++ show (getAccidental n)
         | n < 0     = "flat * " ++ show (negate $ getAccidental n)

instance Alterable Accidental where
  sharpen = succ
  flatten = pred

-- | Magic instance that allow us to write @c sharp@ instead of @sharpen c@.
instance (IsPitch a, Alterable a) => IsPitch (Accidental -> a) where
  fromPitch l 1     = sharpen (fromPitch l)
  fromPitch l (-1)  = flatten (fromPitch l)
-- Requires FlexibleInstances

{-| Lexicographical ordering, comparing the 'd2' component of the
Interval first, as it's tied to the Number which is expected to be
'bigger' than the Quality, assuming ordinary tuning systems
-}
instance Ord Interval where
  Interval a `compare` Interval b = swap a `compare` swap b
    where swap (x,y) = (y,x)







{-
The number portion of an interval (i.e. second, third, etc).

Note that the interval number is always one step larger than number of steps spanned by
the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
over addition:

> number (a + b) = number a + number b - 1
-}

{-
Common pitch representation.

Intervals and pitches can be added using '.+^'. To get the interval between
two pitches, use '.-.'.

Pitches are normally entered using the following literals.

> c d e f g a b

Notes with accidentals can be written by adding the @s@ or @b@ suffices
(or two for double sharps and flats).

> cs, ds, es ...    -- sharp
> cb, db, eb ...    -- flat
> css, dss, ess ... -- double sharp
> cbb, dbb, ebb ... -- double flat

There is also a convenience syntax for entering pitches one octave up or
down, using @'@ and @_@ respectively.

> g a b c'
> d c b_ c

Because of some overloading magic, we can actually write @sharp@ and
@flat@ as /postfix/ functions. This gives a better read:

> cs == c sharp
> db == c flat

You can of course use typical functional transformation of pitch as well.
For example 'sharpen' and 'flatten' are the ordinary (prefix) versions of
'sharp' and 'flat'

> sharpen c             == c sharp       == cs
> flatten d             == d flat        == ds
> (sharpen . sharpen) c == c doubleSharp == css
> (flatten . flatten) d == d doubleFlat  == dss

Note that there is no guarantee that your pitch representation use
enharmonic equivalence, so @cs == db@ may or may not hold.

> c .+^ minor third == eb
> f .-. c           == perfect fourth

Pitches are described by name, accidental and octave number.

> c   == fromIntegral 0
> _P4 == perfect fourth   == interval Perfect 5
> d5  == diminished fifth == diminish (perfect fifth)

-}

{-
A musical interval such as minor third, augmented fifth, duodecim etc.

We include direction in in this definition, so a downward minor third (written @-m3@)
is distinct from an upward minor third (written @m3@). Note that @_P1@ and @-P1@ are
synynoms.

Not to be confused with a mathematical inverval in pitch space, which is called
'Ambitus'. Intervals and pitches form an affine-vector space pair with intervals and
/vectors/ and pitches as /points/. To add an interval to a, use '.+^'. To get the
interval between two pitches, use '.-.'.

> c .+^ minor third == eb
> f .-. c           == perfect fourth

Adding intervals preserves spelling. For example:

> m3 ^+^ _M3 = _P5
> d5 ^+^ _M6 = m10

The scalar type of 'Interval' is 'Int', using '^*' to stack intervals of a certain type
on top of each other. For example @_P5 ^* 2@ is a stack of 2 perfect fifths, or a major
ninth. The 'Num' instance works as expected for '+', 'negate' and 'abs', and
(arbitrarily) uses minor seconds for multiplication. If you find yourself '*', or
'signum' on intervals, consider switching to '*^' or 'normalized'.

Intervals are generally described in terms of 'Quality' and 'Number'. To construct an
interval, use the 'interval' constructor, the utility constructors 'major', 'minor',
'augmented' and 'diminished', or the interval literals:

> m5  == minor   fifth    == interval Minor   5 > _P4 == perfect fourth   == interval
Perfect 5 > d5  == diminished fifth == diminish (perfect fifth)

-}


{-
An accidental is either flat, natural or sharp.

This representation allows for an arbitrary number of flats or sharps rather than just
single and double.

The 'Num' and 'Enum' instances treat 'Accidental' as the number of altered semitones,
i.e. a double flat is @-2@, natural @0@ and so on.

-}

{-
Interval quality is either perfect, major, minor, augmented, and
diminished. This representation allows for an arbitrary number of
augmentations or diminutions, so /augmented/ is represented by @Augmented
1@, /doubly augmented/ by @Augmented 2@ and so on.

The quality of a compound interval is the quality of the simple interval on
which it is based.

-}

{-
An interval represented as a number of octaves, including negative
intervals.

> octaves a = semitones a `div` 12
> steps   a = semitones a `mod` 12
-}

