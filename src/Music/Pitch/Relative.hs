{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    TypeFamilies,
    StandaloneDeriving,
    OverloadedStrings,
    DeriveFoldable #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides overloaded pitch literals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Relative (
    -- * Base types
    -- ** Octaves
    Octaves,
    HasOctaves(..),

    -- ** Steps
    Steps,
    HasSteps(..),
    
    -- ** Semitones
    Semitones,
    semitone, 
    tone, 
    ditone,
    tritone,    
    HasSemitones(..),
    (=:=),
    (/:=),

    -- ** Quality
    Quality(..),    
    HasQuality(..),
    -- invertQuality,
    isPerfect,
    isMajor,
    isMinor,
    isAugmented,
    isDiminished,

    -- ** Accidentals
    Accidental,

    -- ** Alteration
    Alterable(..),
    Augmentable(..),

    -- ** Number
    Number,   
      
    -- ** Name
    Name(..),

    -- * Pitch and interval types
    -- ** Pitch
    Pitch,
    name,
    accidental,

    -- ** Intervals
    Interval,

    -- *** Constructing intervals
    interval,
    perfect,
    major,
    minor,
    augmented,
    diminished,
    doublyAugmented,
    doublyDiminished,

    -- *** Inspecting intervals
    number,
    -- isPerfectUnison,
    isPositive,
    isNegative,

    -- *** Simple and compound intervals
    isSimple,
    isCompound,
    separate,
    simple,
    
    -- *** Intervallic inversion
    invert,

    -- * Utility
    -- ** Spelling
    Spelling,
    spell,
    sharps,
    flats,
    
    isTone,
    isSemitone,
    isTritone,
         
    -- * Literals (TODO move)
    unison,
    prime,
    second,
    third,
    fourth,
    fifth,
    sixth,
    seventh,
    octave,
    -- ninth,
    -- tenth,
    -- twelfth,
    -- thirteenth,
    -- fourteenth,
    -- duodecim,

    d1, _P1, _A1,
    d2, m2, _M2, _A2,
    d3, m3, _M3, _A3,
    d4, _P4, _A4,
    d5, _P5, _A5,
    d6, m6, _M6, _A6,
    d7, m7, _M7, _A7,
    d8, _P8, _A8,    
    
    
)
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute hiding (Octaves(..), octaves)
import Music.Pitch.Literal
import qualified Data.List as List

-- |
-- An interval represented as a number of octaves, including negative intervals.
-- 
-- > octaves a = semitones a `div` 12
-- > steps   a = semitones a `mod` 12
--
newtype Octaves = Octaves { getOctaves :: Integer }
deriving instance Eq Octaves
deriving instance Ord Octaves
instance Show Octaves where
    show (Octaves d) = show d
deriving instance Num Octaves
deriving instance Enum Octaves
deriving instance Real Octaves
deriving instance Integral Octaves
instance HasOctaves Octaves where
    octaves = id

class HasOctaves a where
    -- |
    -- Returns the number of octaves spanned by an interval.
    --
    -- The number of octaves is negative if and only if the interval is negative.
    --
    -- Examples:
    --                   
    -- > octaves (perfect unison)  =  0
    -- > octaves (d5 ^* 4)         =  2
    -- > octaves (-_P8)            =  -1
    --
    octaves :: a -> Octaves


-- |
-- An interval represented as a number of steps in the range /0 ≤ x < 12/.
-- 
-- > octaves a = semitones a `div` 12
-- > steps   a = semitones a `mod` 12
--
newtype Steps = Steps { getSteps :: Integer }
deriving instance Eq Steps
deriving instance Ord Steps
instance Show Steps where
    show (Steps d) = show d
deriving instance Num Steps
deriving instance Enum Steps
deriving instance Real Steps
deriving instance Integral Steps
instance HasSteps Steps where
    steps = id

class HasSteps a where
    steps :: a -> Steps

-- |
-- An interval represented as a number of semitones, including negative
-- intervals, as well as intervals larger than one octave. This representation
-- does not take spelling into account, so for example a major third and a
-- diminished fourth can not be distinguished.
--
-- Intervals that name a number of semitones (i.e. 'semitone', 'tritone') does not
-- have an unequivocal spelling. To convert these to an interval, a 'Spelling' must
-- be provided as in:
--
-- > spell sharps tritone == augmented fourth
-- > spell flats  tritone == diminished fifth
--
newtype Semitones = Semitones { getSemitones :: Integer }
deriving instance Eq Semitones
deriving instance Ord Semitones
instance Show Semitones where
    show (Semitones d) = show d
deriving instance Num Semitones
deriving instance Enum Semitones
deriving instance Real Semitones
deriving instance Integral Semitones
instance HasSemitones Semitones where
    semitones = id

semitone, tone, ditone, tritone :: Semitones

-- | Precisely one semitone.
semitone = 1
-- | Precisely one whole tone, or two semitones.
tone     = 2
-- | Precisely two whole tones, or four semitones.
ditone   = 4
-- | Precisely three whole tones, or six semitones.
tritone  = 6

class HasSemitones a where
    -- |
    -- Returns the number of semitones spanned by an interval.
    --
    -- The number of semitones is negative if and only if the interval is negative.
    --
    -- Examples:
    --                   
    -- > semitones (perfect unison)  =  0
    -- > semitones tritone           =  6
    -- > semitones d5                =  6
    -- > semitones (-_P8)            =  -12
    --
    semitones :: a -> Semitones


infix 4 =:=
infix 4 /:=

-- |
-- Enharmonic equivalence.
--
(=:=) :: HasSemitones a => a -> a -> Bool
a =:= b = semitones a == semitones b

-- |
-- Enharmonic non-equivalence.
--
(/:=) :: HasSemitones a => a -> a -> Bool
a /:= b = semitones a /= semitones b

    
isTone, isSemitone, isTritone :: HasSemitones a => a -> Bool
isTone      = (== tone)     . abs . semitones
isSemitone  = (== semitone) . abs . semitones
isTritone   = (== tritone)  . abs . semitones

-- |
-- The number portion of an interval (i.e. second, third, etc).
--
-- Note that the inverval number is always one step larger than number of steps spanned by
-- the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
-- over addition:
--
-- > number a + number b = number (a + b) + 1
--
newtype Number = Number { getNumber :: Integer }
deriving instance Eq Number
deriving instance Ord Number
instance Show Number where
    show (Number d) = show d
deriving instance Num Number
deriving instance Enum Number
deriving instance Real Number
deriving instance Integral Number

unison  :: Number
prime   :: Number
second  :: Number
third   :: Number
fourth  :: Number
fifth   :: Number
sixth   :: Number
seventh :: Number
octave  :: Number
unison  = 1
prime   = 1
second  = 2
third   = 3
fourth  = 4
fifth   = 5
sixth   = 6
seventh = 7
octave  = 8

-- |
-- Interval quality is either perfect, major, minor, augmented, and
-- diminished. This representation allows for an arbitrary number of
-- augmentation or diminishions, so A is represented by @Augmented 1@, AA by
-- @Augmented 2@ and so on.
-- 
-- The quality of a compound interval is the quality of the simple interval on
-- which it is based.
--
data Quality 
    = Major
    | Minor
    | Perfect
    | Augmented Integer
    | Diminished Integer

deriving instance Eq Quality
deriving instance Ord Quality
instance Show Quality where
    show Major            = "_M"
    show Minor            = "m"
    show Perfect          = "_P"
    show (Augmented n)    = "_" ++ replicate' n 'A'
    show (Diminished n)   = replicate' n 'd'

invertQuality :: Quality -> Quality
invertQuality = go
    where
        go Major            = Minor
        go Minor            = Major
        go Perfect          = Perfect
        go (Augmented n)    = Diminished n
        go (Diminished n)   = Augmented n

class HasQuality a where
    quality :: a -> Quality
instance HasQuality Quality where
    quality = id
class Augmentable a where
    augment :: a -> a
    diminish :: a -> a
class Alterable a where
    sharpen :: a -> a
    flatten :: a -> a

-- instance Augmentable Quality where
--     augment = go
--         where
--             go (Diminished 0)   = Augmented n    -- not unique!
--             go (Diminished n)   = Augmented n
--             go Minor            = Major
--             go Major            = Augmented 1
--             go Perfect          = Augmented 1
--             go (Augmented n)    = Diminished (n)
        

isPerfect :: HasQuality a => a -> Bool
isPerfect a = case quality a of { Perfect -> True ; _ -> False }

isMajor :: HasQuality a => a -> Bool
isMajor a = case quality a of { Major -> True ; _ -> False }

isMinor :: HasQuality a => a -> Bool
isMinor a = case quality a of { Minor -> True ; _ -> False }

isAugmented :: HasQuality a => a -> Bool
isAugmented a = case quality a of { Augmented _ -> True ; _ -> False }

isDiminished :: HasQuality a => a -> Bool
isDiminished a = case quality a of { Diminished _ -> True ; _ -> False }

-- | Convert an offset to a quality.
--
--   This is different for perfect and imperfect interals:
--
--      Imperfect   Perfect
--      ===         === 
-- -3   dd          ddd
-- -2   d           dd
-- -1   m           d
--  0   M           P
--  1   a           a
--  2   aa          aa
--
diffToQuality :: Bool -> Integer -> Quality
diffToQuality = go
    where
        go True 0     = Perfect
        go True n     = if n > 0 then Augmented n else Diminished (negate n)
        go False 0    = Major
        go False (-1) = Minor
        go False n    = if n > 0 then Augmented n else Diminished (negate $ n + 1)

qualityToDiff :: Bool -> Quality -> Integer
qualityToDiff perfect = go
    where
        go (Diminished n)   = negate $ if perfect then n else (n + 1)
        go Minor            = (-1)
        go Perfect          = 0
        go Major            = 0
        go (Augmented n)    = n


-- |
-- An interval is the difference between two pitches, incuding negative
-- intervals.
-- 
-- Intervals and pitches can be added using '.+^'. To get the interval between
-- two pitches, use '.-.'. 
--
-- > c .+^ minor third == eb
-- > f .-. c           == perfect fourth
--
-- Adding intervals preserves spelling. For example:
--
-- > m3 ^+^ _M3 = _P5
-- > d5 ^+^ _M6 = m10 
--
-- The scalar type of intervals are 'Integer', using '^*' to stack intervals
-- of a certain type on top of each other. For example @_P5 ^* 2@ is a stack
-- of 2 perfect fifths. The 'Num' instance works as expected for '+', 'negate'
-- and 'abs', and arbitrarily uses octaves for multiplication. If you find
-- yourself '*', or 'signum' on intervals, consider switching to '^*' or
-- 'normalized'.
--
-- Intervals are generally described in terms of 'Quality' and 'Number'. To
-- construct an interval, use the 'interval' constructor, the utility
-- constructors 'major', 'minor', 'augmented' and 'diminished', or the
-- interval literals:
--
-- > m5  == minor   fifth    == interval Minor   5
-- > _P4 == perfect fourth   == interval Perfect 5
-- > d5  == diminished fifth == diminish (perfect fifth)
--
newtype Interval = Interval { getInterval :: (
    Integer,    -- octaves, may be negative
    Integer,    -- diatonic semitone [0..6]
    Integer     -- chromatic semitone [0..11]
) }

deriving instance Eq Interval
deriving instance Ord Interval
instance Num Interval where
    (+)           = addInterval
    negate        = negateInterval
    abs a         = if isNegative a then negate a else a
    a * b         = fromIntegral (semitones a `div` 12) `stackInterval` b
    signum a      = if isNegative a then (-_P8) else _P8
    fromInteger 0 = _P1
    fromInteger _ = undefined
instance Show Interval where
    show a | isNegative a = "-" ++ show (quality a) ++ show (abs $ number a)
           | otherwise    =        show (quality a) ++ show (abs $ number a)
instance Semigroup Interval where
    (<>)    = addInterval
instance Monoid Interval where
    mempty  = perfect unison
    mappend = addInterval
instance AdditiveGroup Interval where
    zeroV   = perfect unison
    (^+^)   = addInterval
    negateV = negateInterval
instance VectorSpace Interval where
    type Scalar Interval = Integer
    (*^) = stackInterval

instance HasQuality Interval where
    quality (Interval (o, d, c)) 
        | o >= 0    =                 diffToQuality (isPerfectNumber d) (c - diatonicToChromatic d)
        | otherwise = invertQuality $ diffToQuality (isPerfectNumber d) (c - diatonicToChromatic d)

instance Augmentable Interval where
    augment  (Interval (o, d, c)) = Interval (o, d, c + 1)
    diminish (Interval (o, d, c)) = Interval (o, d, c - 1)

instance HasOctaves Interval where
    octaves = fst . separate
instance HasSemitones Interval where
    semitones (Interval (o, d, c)) = Semitones (fromIntegral o * 12 + c)
instance HasSteps Interval where
    steps a = fromIntegral $ semitones a `mod` 12

intervalDiff (Interval (o, d, c)) = c - diatonicToChromatic d

-- |
-- Construct an interval from a quality and number.
--
-- If given 'Perfect' with an imperfect number (such as 3 or 7) a major interval is
-- returned. If given 'Major' or 'Minor' with a perfect number (such as 5), constructs
-- a perfect or diminished interval respectively.
--
interval :: Quality -> Number -> Interval
interval q a = Interval (
    fromIntegral o, 
    fromIntegral n, 
    fromIntegral $ diatonicToChromatic (fromIntegral n) 
        + fromIntegral (qualityToDiff (isPerfectNumber $ fromIntegral n) q)
    )
    where  
        (o, n) = (a - 1) `divMod` 7

interval' :: Int -> Number -> Interval
interval' d a = Interval (
    fromIntegral o, 
    fromIntegral n, 
    fromIntegral $ diatonicToChromatic (fromIntegral n) 
        + fromIntegral d
    )
    where  
        (o, n) = (a - 1) `divMod` 7

{-
-- |
-- The unison interval.
--
unison :: Interval
unison = _P1
-}

-- | Constructs a perfect interval.
--   If given an inperfect number, constructs a major interval.
perfect    = interval Perfect
-- | Constructs a major interval.
--   If given a perfect number, constructs a perfect interval.
major      = interval Major
-- | Constructs a minor interval.
--   If given a perfect number, constructs a diminished interval.
minor      = interval Minor
-- | Constructs an augmented interval.
augmented  = interval (Augmented 1)
-- | Constructs a diminished interval.
diminished = interval (Diminished 1)
-- | Constructs a doubly augmented interval.
doublyAugmented  = interval (Augmented 2)
-- | Constructs a doubly diminished interval.
doublyDiminished = interval (Diminished 2)


negateInterval :: Interval -> Interval
negateInterval (Interval (o, 0, 0))   = Interval (negate o, 0, 0)
negateInterval (Interval (oa, da,ca)) = Interval (negate (oa + 1), invertDiatonic da, invertChromatic ca)

invertDiatonic :: Num a => a -> a
invertDiatonic d  = 7  - d       

invertChromatic :: Num a => a -> a
invertChromatic c = 12 - c
      
addInterval :: Interval -> Interval -> Interval
addInterval (Interval (oa, da,ca)) (Interval (ob, db,cb)) 
    = (Interval (fromIntegral $ oa + ob + fromIntegral carry, steps, chroma))
    where
        (carry, steps) = (da + db) `divMod` 7  
        chroma         = trunc (ca + cb)
        trunc          = if carry > 0 then (`mod` 12) else id

-- |
-- Separate a compound interval into octaves and a simple interval.
--
-- > (perfect octave)^*x + y = z  iff  (x, y) = separate z
--
separate :: Interval -> (Octaves, Interval)
separate (Interval (o, d, c)) = (fromIntegral o, Interval (0, d, c))


-- |
-- Returns the simple part of an interval.
--
-- > (perfect octave)^*x + y = z  iff  y = simple z
--
simple :: Interval -> Interval
simple = snd . separate


-- |
-- Returns the number portion of an interval.
-- 
-- The interval number is negative if and only if the interval is negative.
--
-- See also 'quality', 'octaves' and 'semitones'.
--
number :: Interval -> Number
number (Interval (o, d, c)) = Number (inc $ fromIntegral o * 7 + d)
    where
        inc a = if a >= 0 then succ a else pred a



-- |
-- Returns whether the given interval is simple.
--
-- A simple interval is an positive interval spanning less than one octave.
--
isSimple :: Interval -> Bool
isSimple = (== 0) . octaves

-- |
-- Returns whether the given interval is compound.
--
-- A compound interval is either a negative interval, or a positive interval spanning 
-- more than octave.
--
isCompound :: Interval -> Bool
isCompound = (/= 0) . octaves

isPerfectUnison :: Interval -> Bool
isPerfectUnison a = a == perfect unison

-- |
-- Returns whether the given interval is positive.
--
isPositive :: Interval -> Bool
isPositive (Interval (oa, _, _)) = oa > 0

-- |
-- Returns whether the given interval is negative.
--
isNegative :: Interval -> Bool
isNegative (Interval (oa, _, _)) = oa < 0


stackInterval :: Integer -> Interval -> Interval
stackInterval n a | n >= 0    = mconcat $ replicate (fromIntegral n) a
                  | otherwise = negate $ stackInterval (negate n) a

-- |
-- Intervallic inversion.
--
-- The inversion of a simple interval is determined by the following rules:
-- 
-- * The interval number and the number of its inversion always add up to nine
--   (i.e. 4 + 5 = 9).
--
-- * The inversion of a major interval is a minor interval, and vice versa;
--   the inversion of a perfect interval is also perfect; the inversion of an
--   augmented interval is a diminished interval, and vice versa; the
--   inversion of a doubly augmented interval is a doubly diminished interval,
--   and vice versa.
--
-- The inversion of any compound interval is always the same as the inversion
-- of the simple interval from which it is compounded, i.e.:
--
-- > invert = simple . negate
-- 
invert :: Interval -> Interval   
invert = simple . negate

type Spelling = Semitones -> Number

spell :: HasSemitones a => Spelling -> a -> Interval
spell z = (\s -> Interval (fromIntegral $ s `div` 12, fromIntegral $ z s, fromIntegral s)) .  semitones

isPerfectNumber :: Integer -> Bool
isPerfectNumber 0 = True
isPerfectNumber 1 = False
isPerfectNumber 2 = False
isPerfectNumber 3 = True
isPerfectNumber 4 = True
isPerfectNumber 5 = False
isPerfectNumber 6 = False

diatonicToChromatic :: Integer -> Integer
diatonicToChromatic = go
    where
        go 0 = 0
        go 1 = 2
        go 2 = 4
        go 3 = 5
        go 4 = 7
        go 5 = 9
        go 6 = 11

sharps :: Semitones -> Number
sharps = go
    where
        go 0  = 0
        go 1  = 0
        go 2  = 1
        go 3  = 1
        go 4  = 2
        go 5  = 3
        go 6  = 3
        go 7  = 4
        go 8  = 4
        go 9  = 5
        go 10 = 5
        go 11 = 6

flats :: Semitones -> Number
flats = go
    where
        go 0  = 0
        go 1  = 1
        go 2  = 1
        go 3  = 2
        go 4  = 2
        go 5  = 3
        go 6  = 4
        go 7  = 4
        go 8  = 5
        go 9  = 5
        go 10 = 6
        go 11 = 6




_ = 1 ;                  d1 = Interval (0,0,-1) ; _P1 = Interval (0,0,0)  ; _A1 = Interval (0,0,1)
d2 = Interval (0,1,0)  ; m2 = Interval (0,1,1)  ; _M2 = Interval (0,1,2)  ; _A2 = Interval (0,1,3)
d3 = Interval (0,2,2)  ; m3 = Interval (0,2,3)  ; _M3 = Interval (0,2,4)  ; _A3 = Interval (0,2,5)
_ = 1 ;                  d4 = Interval (0,3,4)  ; _P4 = Interval (0,3,5)  ; _A4 = Interval (0,3,6)
_ = 1 ;                  d5 = Interval (0,4,6)  ; _P5 = Interval (0,4,7)  ; _A5 = Interval (0,4,8)
d6 = Interval (0,5,7)  ; m6 = Interval (0,5,8)  ; _M6 = Interval (0,5,9)  ; _A6 = Interval (0,5,10)
d7 = Interval (0,6,9)  ; m7 = Interval (0,6,10) ; _M7 = Interval (0,6,11) ; _A7 = Interval (0,6,12)
_ = 1 ;                  d8 = Interval (1,0,-1) ; _P8 = Interval (1,0,0)  ; _A8 = Interval (1,0,1)

d9  = d2  + _P8 ; m9  = m2  + _P8 ; _M9  = _M2 + _P8 ; _A9  = _A2 + _P8
d10 = d3  + _P8 ; m10 = m3  + _P8 ; _M10 = _M3 + _P8 ; _A10 = _A3 + _P8












-- |
-- Standard pitch representation.
-- 
-- Intervals and pitches can be added using '.+^'. To get the interval between
-- two pitches, use '.-.'. 
--
-- > c .+^ minor third == eb
-- > f .-. c           == perfect fourth
--
-- Pitches are described by name, accidental and octave number. 
--
-- > c   == fromIntegral 0
-- > _P4 == perfect fourth   == interval Perfect 5
-- > d5  == diminished fifth == diminish (perfect fifth)
--
newtype Pitch = Pitch { getPitch :: Interval }
deriving instance Eq Pitch	 
deriving instance Num Pitch   
deriving instance Ord Pitch	 
instance AffineSpace Pitch where
    type Diff Pitch = Interval
    Pitch a .-. Pitch b = a ^-^ b
    Pitch a .+^ b       = Pitch (a ^+^ b)
instance Show Pitch where
    show p = show (name p) ++ showAccidental (accidental p) ++ showOctave (octaves p)
        where
            showOctave n 
                | n > 0     = replicate' n '\''
                | otherwise = replicate' (negate n) '_'
            showAccidental n 
                | n > 0     = replicate' n 's'
                | otherwise = replicate' (negate n) 'b'


instance Alterable Pitch where
    sharpen (Pitch a) = Pitch (augment a)
    flatten (Pitch a) = Pitch (diminish a)



data Name = C | D | E | F | G | A | B
    deriving (Eq, Ord, Enum)
instance Show Name where
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show A = "a"
    show B = "b"

newtype Accidental = Accidental { getAccidental :: Integer }
deriving instance Eq Accidental
deriving instance Ord Accidental
deriving instance Show Accidental
-- instance Show Accidental where
--     show n | n > 0     = replicate' n 's'
--            | otherwise = replicate' (negate n) 'b'
deriving instance Num Accidental
deriving instance Enum Accidental
deriving instance Real Accidental
deriving instance Integral Accidental
instance Alterable Accidental where
    sharpen = succ
    flatten = pred

asPitch :: Pitch -> Pitch
asPitch = id

-- | 
-- Returns the name (or class) of a pitch.
-- 
-- See also 'octaves', and 'steps' and 'semitones'.
-- 
name :: Pitch -> Name
name = toEnum . fromIntegral . pred . number . simple . getPitch

-- | 
-- Returns the accidental of a pitch.
-- 
-- See also 'octaves', and 'steps' and 'semitones'.
-- 
accidental :: Pitch -> Accidental
accidental = fromIntegral . intervalDiff . simple . getPitch

instance HasOctaves Pitch where
    octaves = octaves . getPitch

instance HasSemitones Pitch where
    semitones = semitones . getPitch

instance HasSteps Pitch where
    steps = steps . getPitch


instance IsPitch Pitch where
    fromPitch (PitchL (c, a, o)) = Pitch (interval' (qual a) (fromIntegral $ c + 1) ^+^ (_P8^* fromIntegral (o - 4)))
        where
            qual Nothing  = 0
            qual (Just n) = round n

-- c = c .+^ (_P8^*5)
-- d = c .+^ _M2

midiNumber :: Pitch -> Integer
midiNumber = getSemitones . semitones . getPitch


{-  
    Some terminology:                                           
        
        newtype Pitch = (PitchClass, Semitones)
            For example (E, Natural)
            We write [c,cs,db..] for [(C, Natural), (C, Sharp), (D, Flat)..]
        
        newtype Interval = (Number, Semitones)
            For example (Augmented, IV)
        
        Interval is the relative representation of pitch 
        
        Pitch is an affine space with Interval as the difference type
            c           .+^ major third = e
            major third ^+^ major third = augmentedFifth
        

        Pitch addition and enhamonic equivalence:



        
        Semitones is the smallest musical unit (Semitones in Western music)
        
        The `semitones` function retrieves the number of Semitones in a pitch, for example
            semitones :: Interval -> Semitones
            semitones major third = 4

        Note that semitones is surjetive. We can define a non-deterministic function `intervals`
            intervals :: Semitones -> [Interval]
            intervals 4 = [majorThird, diminishedFourth]
        Law
            map semitones (intervals a) = replicate n a    for all n > 0
        Lemma
            map semitones (intervals a)
        

        isHemitonic   [1,2,2] = True
        isHemitonic   [2,2,2] = False
        isCohemitonic [1,1,2] = True
        isCohemitonic [1,2,1] = False
        isTritonic ...
        
        A Scale is a [Semitones], for example [2,2,1,2,2,2,1]
            From this we can derive       [2,4,5,7,9,11,12]
        A Scale is a function (Number -> Interval)
        A Scale is a function (Number -> Semitones)

    Tonal
        isConsonance :: Interval -> Bool
        isPerfectConsonance :: Interval -> Bool
        isImperfectConsonance :: Interval -> Bool
        isDissonance :: Interval -> Bool
        isDissonance :: Interval -> Bool
        isHemitonic :: Interval -> Bool
        isTritonic :: Interval -> Bool

        isSemitone :: Interval -> Bool
        isSemitone :: Interval -> Bool
        isLeap :: Interval -> Bool
        isSimple :: Interval -> Bool
        isCompound :: Interval -> Bool
        
        -- TODO simplify etc
        isMelodicDissonance :: Interval -> Bool


    "Post-tonal"
    
        Messiaen
        
        mode1 = [2,2,2,2,2]
        mode2 = [1,2, 1,2, 1,2, 1,2]
        mode3 = [2,1,1, 2,1,1, 2,1,1]
        mode4 = [1,1,3,1,   1,1,3,1]
        mode5 = [1,4,1,     1,4,1]
        mode6 = [2,2,1,1,   2,2,1,1]
        mode7 = [1,1,1,2,1, 1,1,1,2,1]


-}




-- -- Semitone is an enumerated associated type
-- type family Semitone a :: *
-- type family Alteration a :: *
-- 
-- -- A scale is a function :: Semitone a -> a
-- newtype Scale a = Scale { getScale :: [Semitone a] } 
-- -- Eq, Show
-- 
-- semitone :: Scale a -> Semitone a -> a
-- semitone = undefined


-- semitone (Scale xs) p = xs !! (fromIntegral p `mod` length xs)
-- 
-- 
-- fromSemitone :: (Num a, Ord a, Integral b, Num c) => Scale a -> b -> c
-- fromSemitone (Scale xs) p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= fromIntegral p) xs
-- 
-- scaleFromSemitones :: Num a => [a] -> Scale a
-- scaleFromSemitones = Scale . accum
--     where
--         accum = snd . List.mapAccumL add 0
--         add a x = (a + x, a + x)
-- 
-- -- numberOfSemitones :: Scale a -> Int
-- numberOfSemitones = length . getScale
-- 
-- major :: Num a => Scale a
-- major = scaleFromSemitones [0,2,2,1,2,2,2,1]
-- 
-- naturalMinor :: Num a => Scale a
-- naturalMinor = scaleFromSemitones [0,2,1,2,2,1,2,2]
-- 
-- harmonicMinor :: Num a => Scale a
-- harmonicMinor = scaleFromSemitones [0,2,1,2,2,1,3,1]




or' :: (t -> Bool) -> (t -> Bool) -> t -> Bool
or' p q x = p x || q x

replicate' n = replicate (fromIntegral n)