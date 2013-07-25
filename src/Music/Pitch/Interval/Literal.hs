
{-# LANGUAGE NoMonomorphismRestriction #-}

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
-- Provides overloaded interval literals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Interval.Literal (

        IsInterval(..),
        IntervalL(..),

        d1,  _P1,  _A1,
        d2,   m2,  _M2,  _A2,
        d3,   m3,  _M3,  _A3,
        d4,  _P4,  _A4,
        d5,  _P5,  _A5,
        d6,   m6,  _M6,  _A6,
        d7,   m7,  _M7,  _A7,

        d8,  _P8,  _A8,
        d9,   m9,  _M9,  _A9,
        d10,  m10, _M10, _A10,
        d11, _P11, _A11,
        d12, _P12, _A12,
        d13,  m13, _M13, _A13,
        d14,  m14, _M14, _A14,

        d15, _P15,  _A15,
        
  ) where

newtype IntervalL = IntervalL { getPitchL :: (Int, Int, Int) }
    deriving (Eq, Show, Ord)

class IsInterval a where
    fromPitch :: PitchL -> a

instance IsInterval PitchL where

instance IsInterval a => IsInterval (Maybe a) where

instance IsInterval Double where

instance IsInterval Integer where

_ = 1 ;                  d1 = Interval (0,0,-1) ; _P1 = Interval (0,0,0)  ; _A1 = Interval (0,0,1)
d2 = Interval (0,1,0)  ; m2 = Interval (0,1,1)  ; _M2 = Interval (0,1,2)  ; _A2 = Interval (0,1,3)
d3 = Interval (0,2,2)  ; m3 = Interval (0,2,3)  ; _M3 = Interval (0,2,4)  ; _A3 = Interval (0,2,5)
_ = 1 ;                  d4 = Interval (0,3,4)  ; _P4 = Interval (0,3,5)  ; _A4 = Interval (0,3,6)
_ = 1 ;                  d5 = Interval (0,4,6)  ; _P5 = Interval (0,4,7)  ; _A5 = Interval (0,4,8)
d6 = Interval (0,5,7)  ; m6 = Interval (0,5,8)  ; _M6 = Interval (0,5,9)  ; _A6 = Interval (0,5,10)
d7 = Interval (0,6,9)  ; m7 = Interval (0,6,10) ; _M7 = Interval (0,6,11) ; _A7 = Interval (0,6,12)
_ = 1 ;                  d8 = Interval (1,0,-1) ; _P8 = Interval (1,0,0)  ; _A8 = Interval (1,0,1)

