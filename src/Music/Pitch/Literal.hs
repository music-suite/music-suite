
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
-- Provides overloaded pitch literals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Literal (

        IsPitch(..),
        PitchL(..),

        cs'', ds'', es'', fs'', gs'', as'', bs'',
        c'' , d'' , e'' , f'' , g'' , a'' , b'' ,
        cb'', db'', eb'', fb'', gb'', ab'', bb'',

        cs' , ds' , es' , fs' , gs' , as' , bs' ,
        c'  , d'  , e'  , f'  , g'  , a'  , b'  ,
        cb' , db' , eb' , fb' , gb' , ab' , bb' ,

        cs  , ds  , es  , fs  , gs  , as  , bs  ,
        c   , d   , e   , f   , g   , a   , b   ,
        cb  , db  , eb  , fb  , gb  , ab  , bb  ,

        cs_ , ds_ , es_ , fs_ , gs_ , as_ , bs_ ,
        c_  , d_  , e_  , f_  , g_  , a_  , b_  ,
        cb_ , db_ , eb_ , fb_ , gb_ , ab_ , bb_ ,

        cs__, ds__, es__, fs__, gs__, as__, bs__,
        c__ , d__ , e__ , f__ , g__ , a__ , b__ ,
        cb__, db__, eb__, fb__, gb__, ab__, bb__

  ) where

-- Pitch literal, defined as @(class, alteration, octave)@, where
--
--     * @class@      is a pitch class number in @[0..6]@, starting from C.
--
--     * @alteration@ is the number of semitones, i.e. 0 is natural, 1 for sharp 2 for double sharp, -1 for flat and -2 for double flat.
--       Alteration is in 'Maybe' because some pitch representations differ between explicit and explicit accidentals, i.e. a diatonic
--       pitch type may assume @(0,Nothing,4)@ to mean C sharp rather than C.
--
--     * @octave@     is octave number in scientific pitch notation.
--
-- Middle C is represented by the pitch literal @(0, Nothing, 4)@.
--
newtype PitchL = PitchL { getPitchL :: (Int, Maybe Double, Int) }
    deriving (Eq, Show, Ord)

class IsPitch a where
    fromPitch :: PitchL -> a

instance IsPitch PitchL where
    fromPitch = id

instance IsPitch a => IsPitch (Maybe a) where
    fromPitch = Just . fromPitch

instance IsPitch Double where
    fromPitch (PitchL (pc, sem, oct)) = fromIntegral $ semitones sem + diatonic pc + (oct+1) * 12
        where
            semitones = maybe 0 round
            diatonic pc = case pc of
                0 -> 0
                1 -> 2
                2 -> 4
                3 -> 5
                4 -> 7
                5 -> 9
                6 -> 11

instance IsPitch Integer where
    fromPitch (PitchL (pc, sem, oct)) = fromIntegral $ semitones sem + diatonic pc + (oct+1) * 12
        where
            semitones = maybe 0 round
            diatonic pc = case pc of
                0 -> 0
                1 -> 2
                2 -> 4
                3 -> 5
                4 -> 7
                5 -> 9
                6 -> 11

cs'' = fromPitch $ PitchL (0, Just 1, 6)
ds'' = fromPitch $ PitchL (1, Just 1, 6)
es'' = fromPitch $ PitchL (2, Just 1, 6)
fs'' = fromPitch $ PitchL (3, Just 1, 6)
gs'' = fromPitch $ PitchL (4, Just 1, 6)
as'' = fromPitch $ PitchL (5, Just 1, 6)
bs'' = fromPitch $ PitchL (6, Just 1, 6)

c''  = fromPitch $ PitchL (0, Nothing, 6)
d''  = fromPitch $ PitchL (1, Nothing, 6)
e''  = fromPitch $ PitchL (2, Nothing, 6)
f''  = fromPitch $ PitchL (3, Nothing, 6)
g''  = fromPitch $ PitchL (4, Nothing, 6)
a''  = fromPitch $ PitchL (5, Nothing, 6)
b''  = fromPitch $ PitchL (6, Nothing, 6)

cb'' = fromPitch $ PitchL (0, Just (-1), 6)
db'' = fromPitch $ PitchL (1, Just (-1), 6)
eb'' = fromPitch $ PitchL (2, Just (-1), 6)
fb'' = fromPitch $ PitchL (3, Just (-1), 6)
gb'' = fromPitch $ PitchL (4, Just (-1), 6)
ab'' = fromPitch $ PitchL (5, Just (-1), 6)
bb'' = fromPitch $ PitchL (6, Just (-1), 6)

cs'  = fromPitch $ PitchL (0, Just 1, 5)
ds'  = fromPitch $ PitchL (1, Just 1, 5)
es'  = fromPitch $ PitchL (2, Just 1, 5)
fs'  = fromPitch $ PitchL (3, Just 1, 5)
gs'  = fromPitch $ PitchL (4, Just 1, 5)
as'  = fromPitch $ PitchL (5, Just 1, 5)
bs'  = fromPitch $ PitchL (6, Just 1, 5)

c'   = fromPitch $ PitchL (0, Nothing, 5)
d'   = fromPitch $ PitchL (1, Nothing, 5)
e'   = fromPitch $ PitchL (2, Nothing, 5)
f'   = fromPitch $ PitchL (3, Nothing, 5)
g'   = fromPitch $ PitchL (4, Nothing, 5)
a'   = fromPitch $ PitchL (5, Nothing, 5)
b'   = fromPitch $ PitchL (6, Nothing, 5)

cb'  = fromPitch $ PitchL (0, Just (-1), 5)
db'  = fromPitch $ PitchL (1, Just (-1), 5)
eb'  = fromPitch $ PitchL (2, Just (-1), 5)
fb'  = fromPitch $ PitchL (3, Just (-1), 5)
gb'  = fromPitch $ PitchL (4, Just (-1), 5)
ab'  = fromPitch $ PitchL (5, Just (-1), 5)
bb'  = fromPitch $ PitchL (6, Just (-1), 5)

cs   = fromPitch $ PitchL (0, Just 1, 4)
ds   = fromPitch $ PitchL (1, Just 1, 4)
es   = fromPitch $ PitchL (2, Just 1, 4)
fs   = fromPitch $ PitchL (3, Just 1, 4)
gs   = fromPitch $ PitchL (4, Just 1, 4)
as   = fromPitch $ PitchL (5, Just 1, 4)
bs   = fromPitch $ PitchL (6, Just 1, 4)

c    = fromPitch $ PitchL (0, Nothing, 4)
d    = fromPitch $ PitchL (1, Nothing, 4)
e    = fromPitch $ PitchL (2, Nothing, 4)
f    = fromPitch $ PitchL (3, Nothing, 4)
g    = fromPitch $ PitchL (4, Nothing, 4)
a    = fromPitch $ PitchL (5, Nothing, 4)
b    = fromPitch $ PitchL (6, Nothing, 4)

cb   = fromPitch $ PitchL (0, Just (-1), 4)
db   = fromPitch $ PitchL (1, Just (-1), 4)
eb   = fromPitch $ PitchL (2, Just (-1), 4)
fb   = fromPitch $ PitchL (3, Just (-1), 4)
gb   = fromPitch $ PitchL (4, Just (-1), 4)
ab   = fromPitch $ PitchL (5, Just (-1), 4)
bb   = fromPitch $ PitchL (6, Just (-1), 4)

cs_  = fromPitch $ PitchL (0, Just 1, 3)
ds_  = fromPitch $ PitchL (1, Just 1, 3)
es_  = fromPitch $ PitchL (2, Just 1, 3)
fs_  = fromPitch $ PitchL (3, Just 1, 3)
gs_  = fromPitch $ PitchL (4, Just 1, 3)
as_  = fromPitch $ PitchL (5, Just 1, 3)
bs_  = fromPitch $ PitchL (6, Just 1, 3)

c_   = fromPitch $ PitchL (0, Nothing, 3)
d_   = fromPitch $ PitchL (1, Nothing, 3)
e_   = fromPitch $ PitchL (2, Nothing, 3)
f_   = fromPitch $ PitchL (3, Nothing, 3)
g_   = fromPitch $ PitchL (4, Nothing, 3)
a_   = fromPitch $ PitchL (5, Nothing, 3)
b_   = fromPitch $ PitchL (6, Nothing, 3)

cb_  = fromPitch $ PitchL (0, Just (-1), 3)
db_  = fromPitch $ PitchL (1, Just (-1), 3)
eb_  = fromPitch $ PitchL (2, Just (-1), 3)
fb_  = fromPitch $ PitchL (3, Just (-1), 3)
gb_  = fromPitch $ PitchL (4, Just (-1), 3)
ab_  = fromPitch $ PitchL (5, Just (-1), 3)
bb_  = fromPitch $ PitchL (6, Just (-1), 3)

cs__ = fromPitch $ PitchL (0, Just 1, 2)
ds__ = fromPitch $ PitchL (1, Just 1, 2)
es__ = fromPitch $ PitchL (2, Just 1, 2)
fs__ = fromPitch $ PitchL (3, Just 1, 2)
gs__ = fromPitch $ PitchL (4, Just 1, 2)
as__ = fromPitch $ PitchL (5, Just 1, 2)
bs__ = fromPitch $ PitchL (6, Just 1, 2)

c__  = fromPitch $ PitchL (0, Nothing, 2)
d__  = fromPitch $ PitchL (1, Nothing, 2)
e__  = fromPitch $ PitchL (2, Nothing, 2)
f__  = fromPitch $ PitchL (3, Nothing, 2)
g__  = fromPitch $ PitchL (4, Nothing, 2)
a__  = fromPitch $ PitchL (5, Nothing, 2)
b__  = fromPitch $ PitchL (6, Nothing, 2)

cb__ = fromPitch $ PitchL (0, Just (-1), 2)
db__ = fromPitch $ PitchL (1, Just (-1), 2)
eb__ = fromPitch $ PitchL (2, Just (-1), 2)
fb__ = fromPitch $ PitchL (3, Just (-1), 2)
gb__ = fromPitch $ PitchL (4, Just (-1), 2)
ab__ = fromPitch $ PitchL (5, Just (-1), 2)
bb__ = fromPitch $ PitchL (6, Just (-1), 2)

