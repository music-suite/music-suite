
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
    -- ** Semitones
    Semitones,
    tone, 
    semitone, 
    tritone,

    -- ** Interval number
    Number,

    -- ** Interval quality
    Quality,    
    major,
    minor,
    augmented,
    diminished,
    invertQuality,

    HasQuality(..),
    isPerfect,
    isMajor,
    isMinor,
    isAugmented,
    isDiminished,

    -- ** Intervals
    Interval,
    -- *** Constructing intervals
    interval,

    -- *** Inspecing intervals
    number,
    semitones,
    isSimple,
    isCompound,
    isPositive,
    isNegative,
    
    -- *** Transformations
    invert,
    separate,
    
    -- * Spelling
    spell,
    sharps,
    flats,
    
    -- * Harmonic rules
    isTone,
    isSemitone,
    isTritone,
         
    -- ** Literals (TODO move)
    -- prime, 
    -- second, 
    -- third, 
    -- fourth, 
    -- fifth, 
    -- sixth, 
    -- seventh, 
    -- octave,

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
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute
import qualified Data.List as List

-- |
-- An interval represented as a number of semitones.
--
newtype Semitones  = Semitones { getSemitones :: Integer }
deriving instance Eq Semitones
deriving instance Ord Semitones
instance Show Semitones where
    show (Semitones d) = show d
deriving instance Num Semitones
deriving instance Enum Semitones
deriving instance Real Semitones
deriving instance Integral Semitones

semitone, tone, ditone, tritone :: Semitones

-- | A synonym for @1@.
semitone = 1
-- | A synonym for @2@.
tone     = 2
-- | A synonym for @4.
ditone   = 4
-- | A synonym for @6@.
tritone  = 6

isTone      = (== tone)     . semitones
isSemitone  = (== semitone) . semitones
isTritone   = (== tritone)  . semitones

-- |
-- The number portion of an interval (i.e. second, third, etc).
--
-- Note that the inverval number is always one step larger than number of steps spanned by
-- the interval (i.e. a third spans two diatonic steps).
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

-- Bool determines whether the quality refers to a pure interva
--      Impure    Pure
--      ===       === 
-- -3   2dim      3dim
-- -2   dim       2dim
-- -1   minor     dim
-- 0    major     pure
-- 1    aug       aug
-- 2    aug       2aug
data Quality = Quality Bool Integer
deriving instance Eq Quality
deriving instance Ord Quality
-- deriving instance Num Quality
-- deriving instance Enum Quality
-- deriving instance Real Quality
-- deriving instance Integral Quality
instance Show Quality where
    show (Quality True (-3))  = "ddd"
    show (Quality True (-2))  = "dd"
    show (Quality True (-1))  = "d"
    show (Quality True 0)     = "_P"
    show (Quality True 1)     = "_A"
    show (Quality True 2)     = "_AA"
    
    show (Quality False (-3)) = "dd"
    show (Quality False (-2)) = "d"
    show (Quality False (-1)) = "m"
    show (Quality False 0)    = "_M"
    show (Quality False 1)    = "_A"
    show (Quality False 2)    = "_AA"

invertQuality :: Quality -> Quality
invertQuality (Quality True q)  = Quality True (negate q)
invertQuality (Quality False q) = Quality False (negate q - 1)

class HasQuality a where
    quality :: a -> Quality
instance HasQuality Quality where
    quality = id

perfect    = Quality True 0
major      = Quality False 0
minor      = Quality False (-1)
augmented  = Quality False 1
diminished = Quality True (-1) -- TODO this is not unique

isPerfect :: HasQuality a => a -> Bool
isPerfect = (== perfect) . quality

isMajor :: HasQuality a => a -> Bool
isMajor = (== major) . quality

isMinor :: HasQuality a => a -> Bool
isMinor = (== minor) . quality

isAugmented :: HasQuality a => a -> Bool
isAugmented = ((== Quality False 1) `or'` (== Quality True 1)) . quality

isDiminished :: HasQuality a => a -> Bool
isDiminished = ((== Quality False (-2)) `or'` (== Quality True (-1))) . quality

-- |
-- An interval is the difference between two pitches. Note that this
-- definitions includes negative invervals.
-- 
-- Adding intervals preserves spelling. For example:
--
-- > m3 + _M3 = _P5
-- > d5 + _M6 = m10 
--
-- Intervals are generally described in terms of 'Quality' and 'Number'. 
-- To construct an interval, use the 'interval' constructor or the interval literals:
--
-- > m5 _P5 _M7 etc.
--
newtype Interval = Interval { getInterval :: (
    Integer,    -- octaves, may be negative
    Integer,    -- diatonic semitone [0..6]
    Integer     -- chromatic semitone [0..11]
) }

deriving instance Eq Interval
deriving instance Ord Interval
instance Num Interval where
    (+) = addInterval
    (*) = undefined
    negate = negateInterval
    abs = undefined
    signum = undefined
    fromInteger 0 = _P1
    fromInteger _ = undefined
instance Show Interval where
    show a | isNegative a = "-" ++ show (quality a) ++ show (abs $ number a)
           | otherwise    =        show (quality a) ++ show (abs $ number a)
instance Semigroup Interval where
    (<>)    = addInterval
instance Monoid Interval where
    mempty  = _P1
    mappend = addInterval
instance AdditiveGroup Interval where
    zeroV   = _P1
    (^+^)   = addInterval
    negateV = negateInterval
instance VectorSpace Interval where
    type Scalar Interval = Integer
    (*^) = stackInterval

instance HasQuality Interval where
    quality (Interval (o, d, c)) 
        | o >= 0    =                 Quality (isPerfectNumber d) (c - diatonicToChromatic d)
        | otherwise = invertQuality $ Quality (isPerfectNumber d) (c - diatonicToChromatic d)

-- |
-- Construct an interval from a quality and number.
--
-- FIXME bogus
interval :: Quality -> Number -> Interval
interval = go 
    where
        go q n | isMajor q = Interval (0,0,0)
               | isMinor q = Interval (0,0,0)

negateInterval :: Interval -> Interval
negateInterval (Interval (oa, da,ca)) = Interval (negate (oa + 1), invertDiatonic da, invertChromatic ca)

invertDiatonic :: Num a => a -> a
invertDiatonic d  = 7  - d       

invertChromatic :: Num a => a -> a
invertChromatic c = 12 - c
      
addInterval :: Interval -> Interval -> Interval
addInterval (Interval (oa, da,ca)) (Interval (ob, db,cb)) 
    = (Interval (fromIntegral $ oa + ob + fromIntegral carry, semitones, chroma))
    where
        (carry, semitones) = (da + db) `divMod` 7  
        chroma         = (ca + cb) `mod` 12

separate :: Interval -> (Integer, Interval)
separate (Interval (o, d, c)) = (o, Interval (0, d, c))

-- |
-- Returns the number portion of an interval.
-- 
-- The interval number is negative if and only if the interval is negative.
--
number :: Interval -> Number
number (Interval (o, d, c)) = Number (inc $ fromIntegral o * 7 + d)
    where
        inc a = (abs a + 1) * signum a

-- |
-- Returns the number of semitones spanned by an interval.
--
-- The number of semitones is negative if and only if the interval is negative.
--
-- Examples:
--                   
-- > semitones _P1     =  0
-- > semitones m3      =  3
-- > semitones d5      =  6
-- > semitones (-_P8)  =  -12
--
semitones :: Interval -> Semitones
semitones (Interval (o, d, c)) = Semitones (fromIntegral o * 12 + c)

-- Simple invervals are in octave 0, compound invervals are not.
-- For a negative interval, the octave is negative as well.
octave :: Interval -> Integer
octave (Interval (o, d, c)) = o

-- |
-- Returns whether the given interval is simple.
--
-- A simple interval is an positive interval spanning less than one octave.
--
isSimple :: Interval -> Bool
isSimple = (== 0) . octave

-- |
-- Returns whether the given interval is compound.
--
-- A compound interval is either a negative interval, or a positive interval spanning 
-- more than octave.
--
isCompound :: Interval -> Bool
isCompound = (/= 0) . octave

-- |
-- Returns whether the given interval is positive. A simple interval is positive by definition.
--
isPositive :: Interval -> Bool
isPositive (Interval (oa, _, _)) = oa >= 0

-- |
-- Returns whether the given interval is negative.
--
isNegative :: Interval -> Bool
isNegative (Interval (oa, _, _)) = oa < 0



stackInterval :: Integer -> Interval -> Interval
stackInterval n a = mconcat $ replicate (fromIntegral n) a

-- |
-- Interval inversion.
--
-- The inversion of a simple interval is determined as follows:
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
-- of the simple interval from which it is compounded
-- 
invert :: Interval -> Interval   
invert a = let (_, simp) = separate (negate a) in simp


spell :: (Semitones -> Number) -> Interval -> Interval
spell toDia = (\s -> Interval (fromIntegral $ s `div` 12, fromIntegral $ toDia s, fromIntegral s)) .  semitones


-- respell :: Interval -> Interval


_ = 1 ;                  d1 = Interval (0,0,-1) ; _P1 = Interval (0,0,0)  ; _A1 = Interval (0,0,1)
d2 = Interval (0,1,0)  ; m2 = Interval (0,1,1)  ; _M2 = Interval (0,1,2)  ; _A2 = Interval (0,1,3)
d3 = Interval (0,2,2)  ; m3 = Interval (0,2,3)  ; _M3 = Interval (0,2,4)  ; _A3 = Interval (0,2,5)
_ = 1 ;                  d4 = Interval (0,3,4)  ; _P4 = Interval (0,3,5)  ; _A4 = Interval (0,3,6)
_ = 1 ;                  d5 = Interval (0,4,6)  ; _P5 = Interval (0,4,7)  ; _A5 = Interval (0,4,8)
d6 = Interval (0,5,7)  ; m6 = Interval (0,5,8)  ; _M6 = Interval (0,5,9)  ; _A6 = Interval (0,5,10)
d7 = Interval (0,6,9)  ; m7 = Interval (0,6,10) ; _M7 = Interval (0,6,11) ; _A7 = Interval (0,6,12)
_ = 1 ;                  d8 = Interval (1,0,-1) ; _P8 = Interval (1,0,0)  ; _A8 = Interval (1,0,1)

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
