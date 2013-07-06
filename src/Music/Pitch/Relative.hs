
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
    Number,
    Quality,
    Interval,
    quality,
    number,
    invert,
    separate,
    respell,
    
    major,
    minor,
    _pure,
    augmented,
    diminished,
    

    -- ** Predifined
    prime, second, third, fourth, fifth, sixth, seventh, octave,
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
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute
import qualified Data.List as List

-- data    Class      = C | D |E | F | G | A | B
-- data    Alteration = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
-- newtype Pitch      = Pitch { getPitch :: (Class, Alteration) }
-- deriving instance Eq Class
-- deriving instance Ord Class
-- deriving instance Enum Class
-- deriving instance Eq Alteration
-- instance Ord Alteration where { compare a b = compare (fromEnum a) (fromEnum b) }
-- instance Enum Alteration where { toEnum = undefined ; fromEnum = undefined }
-- 


{-
    Number represented by zero-based index
    I.e. prime is 0, second is 1 and so on
-}
newtype Number     = Number { getNumber :: Int }

newtype Quality    = Quality { getQuality :: Int }
deriving instance Eq Number
deriving instance Ord Number
instance Show Number where
    show (Number d) 
        | d >= 0    = show (d + 1)
        | otherwise = error "Number.show: Negative value"
deriving instance Num Number
deriving instance Enum Number
deriving instance Real Number
deriving instance Integral Number

deriving instance Eq Quality
deriving instance Ord Quality
deriving instance Num Quality
deriving instance Enum Quality
deriving instance Real Quality
deriving instance Integral Quality
instance Show Quality where
    show (-2) = "d"
    show (-1) = "m"
    show (0)  = "_P"
    show (1)  = "_M"
    show (2)  = "_A"
    show n | n < 0     = "_" ++ show ((negate $ getQuality n) - 1) ++ "d"
           | otherwise = "_" ++ show (getQuality n - 1) ++ "A"
{-
    Quality is represented by a number as follows:

        ... dd  d  m  P  M  A  AA ...
        ... -3 -2 -1  0  1  2  3 ...

-}

newtype Interval   = Interval { getInterval :: (Quality, Number) }

quality (Interval (q,n)) = q
number (Interval (q,n)) = n

deriving instance Eq Interval
instance Ord Interval where
    Interval (q1,d1) `compare` Interval (q2,d2) = case d1 `compare` d2 of
        EQ -> q1 `compare` q2
        x  -> x
instance Show Interval where
    show (Interval (q,d)) | d >= 0    =        show q ++ show d
                          | otherwise = "-" ++ show q ++ show (negate d)

octaveShift :: Integer -> Interval -> Interval
octaveShift a (Interval (q,n)) = Interval (q, n + 7*fromIntegral a)


{-
    Interval addition can be represented by two infinite matrices (here showing the center):
    
    P :=
        | -5 -4 -3 -2  0 |
        | -4 -3 -2  0  2 |
        | -3 -2  0  2  3 |
        | -2  0  2  3  4 |
        |  0  2  3  4  5 |

    M :=
        | -5 -4 -3 -2 -1  1 |
        | -4 -3 -2 -1  1  2 |
        | -3 -2 -1  1  2  3 |
        | -2 -1  1  2  3  4 |
        | -1  1  2  3  4  5 |
        
    P represents degrees of perfect intervals, M major/minor intervals
    
    We write P(a,b) for P(i+a,j+b), regarding the middle position as (0,0). For example:

    Then we can define a table such as:

        -	1	2	3	4	5	6	7	8
        1	P	M+1	M+1	P	P	M+1	M+1	P
        2	M+1	M+2	P+1	P+1	M+1	M+2	P+1	M+1
        3	M+1	P+1	P+1	M+1	M+1	P+1	M+1	M+1
        4	P	P+1	M+1	M+1	P	M+1	M+1	P
        5	P	M+1	M+1	P	M	M+1	P	P
        6	M+1	M+2	P+1	M+1	M+1	P+1	P+1	M+1
        7	M+1	P+1	M+1	M+1	P	P+1	M+1	M+1
        8	P	M+1	M+1	P	P	M+1	M+1	P

-}                                                    

normQuality :: Number -> Quality -> Quality
normQuality n = if isPure n then normQualityPure else normQualityImpure

isPure :: Number -> Bool
isPure 0 = True
isPure 1 = False
isPure 2 = False
isPure 3 = True
isPure 4 = True
isPure 5 = False
isPure 6 = False
isPure 7 = True

normQualityPure :: Quality -> Quality
normQualityPure q
    | q <  (-1) = q + 1
    | q == 0    = q
    | q >  1    = q - 1
    | otherwise = error "Pure quality can not be 1 or -1"

normQualityImpure :: Quality -> Quality
normQualityImpure q
    | q <  0 = q + 1
    | q >  0 = q
    | otherwise = error "Impure quality can not be 0"
    
Interval (q1,n1) `addSimple` Interval (q2,n2) = Interval (addQuality q1 n1 q2 n2, n1 + n2)

addQuality :: Quality -> Number -> Quality -> Number -> Quality
addQuality q1 n1 q2 n2 = Quality $ lookup2 table (getQuality $ normQuality n1 q1) (getQuality $ normQuality n2 q2) 
    where
          table = [
                [ pt  , mt1 , mt1 , pt  , pt  , mt1 , mt1 , pt  ],
                [ mt1 , mt2 , pt1 , pt1 , mt1 , mt2 , pt1 , mt1 ],
                [ mt1 , pt1 , pt1 , mt1 , mt1 , pt1 , mt1 , mt1 ],
                [ pt  , pt1 , mt1 , mt1 , pt  , mt1 , mt1 , pt  ],
                [ pt  , mt1 , mt1 , pt  , mt  , mt1 , pt  , pt  ],
                [ mt1 , mt2 , pt1 , mt1 , mt1 , pt1 , pt1 , mt1 ],
                [ mt1 , pt1 , mt1 , mt1 , pt  , pt1 , mt1 , mt1 ],
                [ pt  , mt1 , mt1 , pt  , pt  , mt1 , mt1 , pt  ]
            ]
          lookup2 a = (a !! fromIntegral n2) !! fromIntegral n1
          
          -- the P and M matrixes with relevant translations
          pt, mt, mt1, mt2 :: Mat Int

          pt x y = let n = x + y in n + (1 * signum n)
          mt x y = let n = x + y in if (n >= 0) then n + 1 else n
          pt1 = moveX 1 pt
          mt1 = moveX 1 mt
          mt2 = moveX 2 mt

type Mat a = a -> a -> a

moveX :: Num a  => a      -> Mat a -> Mat a
moveY :: Num a  => a      -> Mat a -> Mat a
moveXY :: Num a => a -> a -> Mat a -> Mat a
moveX  m z   = \x y -> z (x - m) y
moveY  m z   = \x y -> z x       (y - m)
moveXY m n z = \x y -> z (x - m) (y - n)

{-
putMatrix :: (Eq a, Num a, Ord a, Enum a, Show a) => Mat a -> IO ()
putMatrix = putStrLn . showMatrix 5 5 . moveXY 2 2

showMatrix :: (Eq a, Num a, Ord a, Enum a, Show a) => a -> a -> Mat a -> String
showMatrix w z mat = unlines $ do
    y <- [0..z-1]
    return $ (++ "|") $ ("| " ++) $ mconcat $ do
        x <- [0..w-1]
        return $ padL 3 $ showN $ mat x y
    where
        showN n | n >= 0    = " " ++ show n
                | otherwise = show n
-}




-- dim, pur, maj, minor, aug :: Quality
-- dim = (-2)
-- minor = (-1)
-- pur = 0
-- maj = 1
-- aug = 2

major :: Number -> Interval
major n | isPure n  = error $ "major: Invalid number: " ++ show n
        | otherwise = Interval (1,n)
minor :: Number -> Interval
minor n | isPure n  = error $ "minor: Invalid number: " ++ show n
        | otherwise = Interval (-1,n)
_pure :: Number -> Interval
_pure n | not (isPure n)  = error $ "_pure: Invalid number: " ++ show n
       | otherwise = Interval (0,n)

augmented :: Number -> Interval
augmented n = Interval (2,n)

diminished :: Number -> Interval
diminished n = Interval (-2,n)



prime, second, third, fourth, fifth, sixth, seventh, octave :: Number
prime = 0
second = 1
third = 2
fourth = 3
fifth = 4
sixth = 5
seventh = 6
octave = 7

d1 = Interval (-2,0) ; _P1 = Interval (-0,0) ; _A1 = Interval (2,0)
d4 = Interval (-2,3) ; _P4 = Interval (-0,3) ; _A4 = Interval (2,3)
d5 = Interval (-2,4) ; _P5 = Interval (-0,4) ; _A5 = Interval (2,4)
d8 = Interval (-2,7) ; _P8 = Interval (-0,7) ; _A8 = Interval (2,7)
d2 = Interval (-2,1) ; m2  = Interval (-1,1) ; _M2 = Interval (1,1) ; _A2 = Interval (2,1)
d3 = Interval (-2,2) ; m3  = Interval (-1,2) ; _M3 = Interval (1,2) ; _A3 = Interval (2,2)
d6 = Interval (-2,5) ; m6  = Interval (-1,5) ; _M6 = Interval (1,5) ; _A6 = Interval (2,5)
d7 = Interval (-2,6) ; m7  = Interval (-1,6) ; _M7 = Interval (1,6) ; _A7 = Interval (2,6)


-- TODO subtraction
instance Num Interval where
    a + b  = let
        (ao, as) = separate a
        (bo, bs) = separate b
        in addSimple as bs
        -- TODO
    a * b  = error "Interval.(*)"
    abs (Interval (q,d)) | d < 0     = Interval (q, negate d)
                         | otherwise = Interval (q, d)
    negate (Interval (q,d)) = Interval (q, negate d)

    fromInteger = error "Interval.fromInteger"
    signum      = error "Interval.fromInteger"

-- |
-- Separate a compound interval
-- Returns a (possibly negative) number of octaves
-- For simple interval @a@, returns @(0,a)@.
separate :: Interval -> (Integer, Interval)
separate (Interval (q,d)) = let (octaves, steps) = d `divMod` 7
                             in (fromIntegral octaves, Interval (if octaves < 0 then negate q else q, fromIntegral steps))


                              
invert :: Interval -> Interval   
invert (Interval (0,0)) = Interval (0,7)
invert a = let (_, simp) = separate (negate a) in simp



-- TODO
-- | rewrite as pure/major/minor (or aug4/dim5 for tritones)
respell :: Interval -> Interval
respell = error "simplify: Not implemented"


-- type Function = [Interval]

-- majorTriad :: Function
-- majorTriad = [_M3, m3]
-- 
-- minorTriad :: Function
-- minorTriad = [m3, _M3]
-- 
-- diminished :: Function
-- diminished = [m3, m3, m3]


{-  
    Some terminology:                                           
        
        newtype Pitch = (PitchClass, Steps)
            For example (E, Natural)
            We write [c,cs,db..] for [(C, Natural), (C, Sharp), (D, Flat)..]
        
        newtype Interval = (Number, Steps)
            For example (Augmented, IV)
        
        Interval is the relative representation of pitch 
        
        Pitch is an affine space with Interval as the difference type
            c           .+^ major third = e
            major third ^+^ major third = augmentedFifth
        

        Pitch addition and enhamonic equivalence:



        
        Steps is the smallest musical unit (Semitones in Western music)
        
        The `steps` function retrieves the number of Steps in a pitch, for example
            steps :: Interval -> Steps
            steps major third = 4

        Note that steps is surjetive. We can define a non-deterministic function `intervals`
            intervals :: Steps -> [Interval]
            intervals 4 = [majorThird, diminishedFourth]
        Law
            map steps (intervals a) = replicate n a    for all n > 0
        Lemma
            map steps (intervals a)
        

        isHemitonic   [1,2,2] = True
        isHemitonic   [2,2,2] = False
        isCohemitonic [1,1,2] = True
        isCohemitonic [1,2,1] = False
        isTritonic ...
        
        A Scale is a [Steps], for example [2,2,1,2,2,2,1]
            From this we can derive       [2,4,5,7,9,11,12]
        A Scale is a function (Number -> Interval)
        A Scale is a function (Number -> Steps)

    Tonal
        isConsonance :: Interval -> Bool
        isPerfectConsonance :: Interval -> Bool
        isImperfectConsonance :: Interval -> Bool
        isDissonance :: Interval -> Bool
        isDissonance :: Interval -> Bool
        isHemitonic :: Interval -> Bool
        isTritonic :: Interval -> Bool

        isSemitone :: Interval -> Bool
        isStep :: Interval -> Bool
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




-- -- Step is an enumerated associated type
-- type family Step a :: *
-- type family Alteration a :: *
-- 
-- -- A scale is a function :: Step a -> a
-- newtype Scale a = Scale { getScale :: [Step a] } 
-- -- Eq, Show
-- 
-- step :: Scale a -> Step a -> a
-- step = undefined


-- step (Scale xs) p = xs !! (fromIntegral p `mod` length xs)
-- 
-- 
-- fromStep :: (Num a, Ord a, Integral b, Num c) => Scale a -> b -> c
-- fromStep (Scale xs) p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= fromIntegral p) xs
-- 
-- scaleFromSteps :: Num a => [a] -> Scale a
-- scaleFromSteps = Scale . accum
--     where
--         accum = snd . List.mapAccumL add 0
--         add a x = (a + x, a + x)
-- 
-- -- numberOfSteps :: Scale a -> Int
-- numberOfSteps = length . getScale
-- 
-- major :: Num a => Scale a
-- major = scaleFromSteps [0,2,2,1,2,2,2,1]
-- 
-- naturalMinor :: Num a => Scale a
-- naturalMinor = scaleFromSteps [0,2,1,2,2,1,2,2]
-- 
-- harmonicMinor :: Num a => Scale a
-- harmonicMinor = scaleFromSteps [0,2,1,2,2,1,3,1]




padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

