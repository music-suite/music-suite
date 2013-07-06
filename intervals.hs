
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    TypeFamilies,
    StandaloneDeriving,
    OverloadedStrings,
    DeriveFoldable #-}

module Intervals where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute
import qualified Data.List as List


newtype Steps     = Steps { getSteps :: Integer }
deriving instance Eq Steps
deriving instance Ord Steps
instance Show Steps where
    show (Steps d) = show d
deriving instance Num Steps
deriving instance Enum Steps
deriving instance Real Steps
deriving instance Integral Steps


newtype Number     = Number { getNumber :: Integer }
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

invertQuality (Quality True q)  = Quality True (negate q)
invertQuality (Quality False q) = Quality False (negate q - 1)


-- TODO strange behaviour for negative values
major :: Number -> Interval
major a | isPure (fromIntegral $ (a-1) `mod` 7)  = error $ "major: Invalid number: " ++ show a
        | otherwise                = let n = a - 1 in Interval (
            fromIntegral $ n `div` 7, 
            fromIntegral $ n `mod` 7, 
            diatonicToChromatic $ fromIntegral $ n `mod` 7)
minor :: Number -> Interval
minor a | isPure (fromIntegral $ (a-1) `mod` 7) = error $ "minor: Invalid number: " ++ show a
        | otherwise                = let n = a - 1 in Interval (
            fromIntegral $ n `div` 7, 
            fromIntegral $ n `mod` 7, 
            pred $ diatonicToChromatic $ fromIntegral $ n `mod` 7)
        
-- minor :: Number -> Interval
-- minor n | isPure n  = error $ "minor: Invalid number: " ++ show n
--         | otherwise = Interval (-1,n)


type Octave = Integer
type Diatonic = Integer
type Chromatic = Integer
newtype Interval = Interval { getInterval :: (
    Octave,     -- octaves, may be negative
    Diatonic,   -- diatonic step [0..6]
    Chromatic   -- chromatic step [0..11]
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
    show a | intervalNegative a = "-" ++ show (quality a) ++ show (abs $ number a)
           | otherwise          = show (quality a) ++ show (abs $ number a)
instance Semigroup Interval where
    (<>)    = addInterval
instance Monoid Interval where
    mempty  = _P1
    mappend = addInterval
instance AdditiveGroup Interval where
    zeroV   = _P1
    (^+^)   = addInterval
    negateV = negateInterval

-- rquality :: Interval -> Integer
-- rquality (Interval (o, d, c)) = (c - diatonicToChromatic d)

quality :: Interval -> Quality
quality (Interval (o, d, c)) 
    | o >= 0    =                 Quality (isPure d) (c - diatonicToChromatic d)
    | otherwise = invertQuality $ Quality (isPure d) (c - diatonicToChromatic d)

-- |
-- Number of diatonic steps (i.e. 1 for a prime, 2 for second etc).
-- For a negative interval, its number is negative as well.
number :: Interval -> Number
number (Interval (o, d, c)) = Number (inc $ o * 7 + d)
    where
        inc a = (abs a + 1) * signum a

-- For a negative interval, the steps is negative as well.
steps :: Interval -> Steps
steps (Interval (o, d, c)) = Steps (o * 12 + c)

-- |
-- In which octave is the inverval.
-- Simple invervals are in octave 0, compound invervals are not.
-- For a negative interval, the octave is negative as well.
octave :: Interval -> Octave
octave (Interval (o, d, c)) = o

isSimple :: Interval -> Bool
isSimple = (== 0) . octave

isCompound :: Interval -> Bool
isCompound = (/= 0) . octave



intervalNegative :: Interval -> Bool
intervalNegative (Interval (oa, _, _)) = oa < 0

negateInterval :: Interval -> Interval
negateInterval (Interval (oa, da,ca)) = Interval (negate (oa + 1), invertDiatonic da, invertChromatic ca)

invertDiatonic :: Num a => a -> a
invertDiatonic d  = 7  - d       

invertChromatic :: Num a => a -> a
invertChromatic c = 12 - c
      
addInterval :: Interval -> Interval -> Interval
addInterval (Interval (oa, da,ca)) (Interval (ob, db,cb)) 
    = (Interval (oa + ob + carry, steps, chroma))
    where
        (carry, steps) = (da + db) `divMod` 7  
        chroma         = (ca + cb) `mod` 12

separate :: Interval -> (Integer, Interval)
separate (Interval (o, d, c)) = (o, Interval (0, d, c))

invert :: Interval -> Interval   
-- invert (Interval (0,0)) = Interval (0,7)
invert a = let (_, simp) = separate (negate a) in simp


spell :: Integral a => (Steps -> a) -> Interval -> Interval
spell toDia = (\s -> Interval (fromIntegral $ s `div` 12, fromIntegral $ toDia s, fromIntegral s)) .  steps


-- respell :: Interval -> Interval


_ = 1 ;                  d1 = Interval (0,0,-1) ; _P1 = Interval (0,0,0)  ; _A1 = Interval (0,0,1)
d2 = Interval (0,1,0)  ; m2 = Interval (0,1,1)  ; _M2 = Interval (0,1,2)  ; _A2 = Interval (0,1,3)
d3 = Interval (0,2,2)  ; m3 = Interval (0,2,3)  ; _M3 = Interval (0,2,4)  ; _A3 = Interval (0,2,5)
_ = 1 ;                  d4 = Interval (0,3,4)  ; _P4 = Interval (0,3,5)  ; _A4 = Interval (0,3,6)
_ = 1 ;                  d5 = Interval (0,4,6)  ; _P5 = Interval (0,4,7)  ; _A5 = Interval (0,4,8)
d6 = Interval (0,5,7)  ; m6 = Interval (0,5,8)  ; _M6 = Interval (0,5,9)  ; _A6 = Interval (0,5,10)
d7 = Interval (0,6,9)  ; m7 = Interval (0,6,10) ; _M7 = Interval (0,6,11) ; _A7 = Interval (0,6,12)
_ = 1 ;                  d8 = Interval (1,0,-1) ; _P8 = Interval (1,0,0)  ; _A8 = Interval (1,0,1)

isPure :: Integer -> Bool
isPure 0 = True
isPure 1 = False
isPure 2 = False
isPure 3 = True
isPure 4 = True
isPure 5 = False
isPure 6 = False

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

sharps :: Steps -> Integer
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

flats :: Steps -> Integer
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
