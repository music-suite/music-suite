
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

module Music.Pitch.Literal.Pitch (

        -- * IsPitch class
        IsPitch(..),

        -- * Literal values

        -- ** Four octaves up
        cs'''', ds'''', es'''', fs'''', gs'''', as'''', bs'''',
        c'''' , d'''' , e'''' , f'''' , g'''' , a'''' , b'''' ,
        cb'''', db'''', eb'''', fb'''', gb'''', ab'''', bb'''',

        -- ** Three octaves up
        cs''', ds''', es''', fs''', gs''', as''', bs''',
        c''' , d''' , e''' , f''' , g''' , a''' , b''' ,
        cb''', db''', eb''', fb''', gb''', ab''', bb''',

        -- ** Two octaves up
        cs'', ds'', es'', fs'', gs'', as'', bs'',
        c'' , d'' , e'' , f'' , g'' , a'' , b'' ,
        cb'', db'', eb'', fb'', gb'', ab'', bb'',

        -- ** One octave up
        cs' , ds' , es' , fs' , gs' , as' , bs' ,
        c'  , d'  , e'  , f'  , g'  , a'  , b'  ,
        cb' , db' , eb' , fb' , gb' , ab' , bb' ,

        -- ** Standard octave
        cs  , ds  , es  , fs  , gs  , as  , bs  ,
        c   , d   , e   , f   , g   , a   , b   ,
        cb  , db  , eb  , fb  , gb  , ab  , bb  ,

        -- ** One octave down
        cs_ , ds_ , es_ , fs_ , gs_ , as_ , bs_ ,
        c_  , d_  , e_  , f_  , g_  , a_  , b_  ,
        cb_ , db_ , eb_ , fb_ , gb_ , ab_ , bb_ ,

        -- ** Two octaves down
        cs__, ds__, es__, fs__, gs__, as__, bs__,
        c__ , d__ , e__ , f__ , g__ , a__ , b__ ,
        cb__, db__, eb__, fb__, gb__, ab__, bb__,

        -- ** Three octaves down
        cs___, ds___, es___, fs___, gs___, as___, bs___,
        c___ , d___ , e___ , f___ , g___ , a___ , b___ ,
        cb___, db___, eb___, fb___, gb___, ab___, bb___,

        -- ** Four octaves down
        cs____, ds____, es____, fs____, gs____, as____, bs____,
        c____ , d____ , e____ , f____ , g____ , a____ , b____ ,
        cb____, db____, eb____, fb____, gb____, ab____, bb____,

  ) where

import           Control.Applicative
import           Data.Fixed
import           Data.Int
import           Data.Ratio
import           Data.Semigroup
import           Data.Word
import Data.AffineSpace ((.-.))

import Music.Pitch.Common.Types

-- Pitch literal, defined as @(class, alteration, octave)@, where
--
--     * @class@      is a pitch class number in @[0..6]@, starting from C.
--
--     * @alteration@ is the number of semitones, i.e. 0 is natural, 1 for sharp 2 for double sharp, -1 for flat and -2 for double flat.
--       Alteration is in 'Maybe' because some pitch representations differ between explicit and explicit accidentals, i.e. a diatonic
--       pitch type may assume @(0,Nothing,...)@ to mean C sharp rather than C.
--
--     * @octave@     is octave number in scientific pitch notation - 4.
--
-- Middle C is represented by the pitch literal @(0, Nothing, 0)@.
--
-- newtype PitchL = PitchL { getPitchL :: (Int, Maybe Double, Int) }
    -- deriving (Eq, Show, Ord)

class IsPitch a where
    fromPitch :: Pitch -> a

instance IsPitch a => IsPitch (Maybe a) where
    fromPitch = pure . fromPitch

instance IsPitch a => IsPitch (First a) where
    fromPitch = pure . fromPitch

instance IsPitch a => IsPitch (Last a) where
    fromPitch = pure . fromPitch

instance IsPitch a => IsPitch [a] where
    fromPitch = pure . fromPitch

instance (Monoid b, IsPitch a) => IsPitch (b, a) where
    fromPitch = pure . fromPitch

-- TODO clean by inlining this whole thing or similar
viaPitchL :: (Int, Int, Int) -> Pitch
viaPitchL (pc, sem, oct) = Pitch $ mkInterval' sem (oct * 7 + pc)
  where
    mkInterval' diff diatonic = Interval (diatonicToChromatic (fromIntegral diatonic) + fromIntegral diff, fromIntegral diatonic)

    diatonicToChromatic :: DiatonicSteps -> ChromaticSteps
    diatonicToChromatic d = fromIntegral $ (octaves*12) + go restDia
        where
            -- restDia is always in [0..6]
            (octaves, restDia) = fromIntegral d `divMod` 7
            go = ([0,2,4,5,7,9,11] !!)


cs''''    = fromPitch $ viaPitchL (0, 1, 4)
ds''''    = fromPitch $ viaPitchL (1, 1, 4)
es''''    = fromPitch $ viaPitchL (2, 1, 4)
fs''''    = fromPitch $ viaPitchL (3, 1, 4)
gs''''    = fromPitch $ viaPitchL (4, 1, 4)
as''''    = fromPitch $ viaPitchL (5, 1, 4)
bs''''    = fromPitch $ viaPitchL (6, 1, 4)

c''''     = fromPitch $ viaPitchL (0, 0, 4)
d''''     = fromPitch $ viaPitchL (1, 0, 4)
e''''     = fromPitch $ viaPitchL (2, 0, 4)
f''''     = fromPitch $ viaPitchL (3, 0, 4)
g''''     = fromPitch $ viaPitchL (4, 0, 4)
a''''     = fromPitch $ viaPitchL (5, 0, 4)
b''''     = fromPitch $ viaPitchL (6, 0, 4)

cb''''    = fromPitch $ viaPitchL (0, (-1), 4)
db''''    = fromPitch $ viaPitchL (1, (-1), 4)
eb''''    = fromPitch $ viaPitchL (2, (-1), 4)
fb''''    = fromPitch $ viaPitchL (3, (-1), 4)
gb''''    = fromPitch $ viaPitchL (4, (-1), 4)
ab''''    = fromPitch $ viaPitchL (5, (-1), 4)
bb''''    = fromPitch $ viaPitchL (6, (-1), 4)

cs'''     = fromPitch $ viaPitchL (0, 1, 3)
ds'''     = fromPitch $ viaPitchL (1, 1, 3)
es'''     = fromPitch $ viaPitchL (2, 1, 3)
fs'''     = fromPitch $ viaPitchL (3, 1, 3)
gs'''     = fromPitch $ viaPitchL (4, 1, 3)
as'''     = fromPitch $ viaPitchL (5, 1, 3)
bs'''     = fromPitch $ viaPitchL (6, 1, 3)

c'''      = fromPitch $ viaPitchL (0, 0, 3)
d'''      = fromPitch $ viaPitchL (1, 0, 3)
e'''      = fromPitch $ viaPitchL (2, 0, 3)
f'''      = fromPitch $ viaPitchL (3, 0, 3)
g'''      = fromPitch $ viaPitchL (4, 0, 3)
a'''      = fromPitch $ viaPitchL (5, 0, 3)
b'''      = fromPitch $ viaPitchL (6, 0, 3)

cb'''     = fromPitch $ viaPitchL (0, (-1), 3)
db'''     = fromPitch $ viaPitchL (1, (-1), 3)
eb'''     = fromPitch $ viaPitchL (2, (-1), 3)
fb'''     = fromPitch $ viaPitchL (3, (-1), 3)
gb'''     = fromPitch $ viaPitchL (4, (-1), 3)
ab'''     = fromPitch $ viaPitchL (5, (-1), 3)
bb'''     = fromPitch $ viaPitchL (6, (-1), 3)

cs''      = fromPitch $ viaPitchL (0, 1, 2)
ds''      = fromPitch $ viaPitchL (1, 1, 2)
es''      = fromPitch $ viaPitchL (2, 1, 2)
fs''      = fromPitch $ viaPitchL (3, 1, 2)
gs''      = fromPitch $ viaPitchL (4, 1, 2)
as''      = fromPitch $ viaPitchL (5, 1, 2)
bs''      = fromPitch $ viaPitchL (6, 1, 2)

c''       = fromPitch $ viaPitchL (0, 0, 2)
d''       = fromPitch $ viaPitchL (1, 0, 2)
e''       = fromPitch $ viaPitchL (2, 0, 2)
f''       = fromPitch $ viaPitchL (3, 0, 2)
g''       = fromPitch $ viaPitchL (4, 0, 2)
a''       = fromPitch $ viaPitchL (5, 0, 2)
b''       = fromPitch $ viaPitchL (6, 0, 2)

cb''      = fromPitch $ viaPitchL (0, (-1), 2)
db''      = fromPitch $ viaPitchL (1, (-1), 2)
eb''      = fromPitch $ viaPitchL (2, (-1), 2)
fb''      = fromPitch $ viaPitchL (3, (-1), 2)
gb''      = fromPitch $ viaPitchL (4, (-1), 2)
ab''      = fromPitch $ viaPitchL (5, (-1), 2)
bb''      = fromPitch $ viaPitchL (6, (-1), 2)

cs'       = fromPitch $ viaPitchL (0, 1, 1)
ds'       = fromPitch $ viaPitchL (1, 1, 1)
es'       = fromPitch $ viaPitchL (2, 1, 1)
fs'       = fromPitch $ viaPitchL (3, 1, 1)
gs'       = fromPitch $ viaPitchL (4, 1, 1)
as'       = fromPitch $ viaPitchL (5, 1, 1)
bs'       = fromPitch $ viaPitchL (6, 1, 1)

c'        = fromPitch $ viaPitchL (0, 0, 1)
d'        = fromPitch $ viaPitchL (1, 0, 1)
e'        = fromPitch $ viaPitchL (2, 0, 1)
f'        = fromPitch $ viaPitchL (3, 0, 1)
g'        = fromPitch $ viaPitchL (4, 0, 1)
a'        = fromPitch $ viaPitchL (5, 0, 1)
b'        = fromPitch $ viaPitchL (6, 0, 1)

cb'       = fromPitch $ viaPitchL (0, (-1), 1)
db'       = fromPitch $ viaPitchL (1, (-1), 1)
eb'       = fromPitch $ viaPitchL (2, (-1), 1)
fb'       = fromPitch $ viaPitchL (3, (-1), 1)
gb'       = fromPitch $ viaPitchL (4, (-1), 1)
ab'       = fromPitch $ viaPitchL (5, (-1), 1)
bb'       = fromPitch $ viaPitchL (6, (-1), 1)

cs        = fromPitch $ viaPitchL (0, 1, 0)
ds        = fromPitch $ viaPitchL (1, 1, 0)
es        = fromPitch $ viaPitchL (2, 1, 0)
fs        = fromPitch $ viaPitchL (3, 1, 0)
gs        = fromPitch $ viaPitchL (4, 1, 0)
as        = fromPitch $ viaPitchL (5, 1, 0)
bs        = fromPitch $ viaPitchL (6, 1, 0)

c         = fromPitch $ viaPitchL (0, 0, 0)
d         = fromPitch $ viaPitchL (1, 0, 0)
e         = fromPitch $ viaPitchL (2, 0, 0)
f         = fromPitch $ viaPitchL (3, 0, 0)
g         = fromPitch $ viaPitchL (4, 0, 0)
a         = fromPitch $ viaPitchL (5, 0, 0)
b         = fromPitch $ viaPitchL (6, 0, 0)

cb        = fromPitch $ viaPitchL (0, (-1), 0)
db        = fromPitch $ viaPitchL (1, (-1), 0)
eb        = fromPitch $ viaPitchL (2, (-1), 0)
fb        = fromPitch $ viaPitchL (3, (-1), 0)
gb        = fromPitch $ viaPitchL (4, (-1), 0)
ab        = fromPitch $ viaPitchL (5, (-1), 0)
bb        = fromPitch $ viaPitchL (6, (-1), 0)

cs_       = fromPitch $ viaPitchL (0, 1, -1)
ds_       = fromPitch $ viaPitchL (1, 1, -1)
es_       = fromPitch $ viaPitchL (2, 1, -1)
fs_       = fromPitch $ viaPitchL (3, 1, -1)
gs_       = fromPitch $ viaPitchL (4, 1, -1)
as_       = fromPitch $ viaPitchL (5, 1, -1)
bs_       = fromPitch $ viaPitchL (6, 1, -1)

c_        = fromPitch $ viaPitchL (0, 0, -1)
d_        = fromPitch $ viaPitchL (1, 0, -1)
e_        = fromPitch $ viaPitchL (2, 0, -1)
f_        = fromPitch $ viaPitchL (3, 0, -1)
g_        = fromPitch $ viaPitchL (4, 0, -1)
a_        = fromPitch $ viaPitchL (5, 0, -1)
b_        = fromPitch $ viaPitchL (6, 0, -1)

cb_       = fromPitch $ viaPitchL (0, (-1), -1)
db_       = fromPitch $ viaPitchL (1, (-1), -1)
eb_       = fromPitch $ viaPitchL (2, (-1), -1)
fb_       = fromPitch $ viaPitchL (3, (-1), -1)
gb_       = fromPitch $ viaPitchL (4, (-1), -1)
ab_       = fromPitch $ viaPitchL (5, (-1), -1)
bb_       = fromPitch $ viaPitchL (6, (-1), -1)

cs__      = fromPitch $ viaPitchL (0, 1, -2)
ds__      = fromPitch $ viaPitchL (1, 1, -2)
es__      = fromPitch $ viaPitchL (2, 1, -2)
fs__      = fromPitch $ viaPitchL (3, 1, -2)
gs__      = fromPitch $ viaPitchL (4, 1, -2)
as__      = fromPitch $ viaPitchL (5, 1, -2)
bs__      = fromPitch $ viaPitchL (6, 1, -2)

c__       = fromPitch $ viaPitchL (0, 0, -2)
d__       = fromPitch $ viaPitchL (1, 0, -2)
e__       = fromPitch $ viaPitchL (2, 0, -2)
f__       = fromPitch $ viaPitchL (3, 0, -2)
g__       = fromPitch $ viaPitchL (4, 0, -2)
a__       = fromPitch $ viaPitchL (5, 0, -2)
b__       = fromPitch $ viaPitchL (6, 0, -2)

cb__      = fromPitch $ viaPitchL (0, (-1), -2)
db__      = fromPitch $ viaPitchL (1, (-1), -2)
eb__      = fromPitch $ viaPitchL (2, (-1), -2)
fb__      = fromPitch $ viaPitchL (3, (-1), -2)
gb__      = fromPitch $ viaPitchL (4, (-1), -2)
ab__      = fromPitch $ viaPitchL (5, (-1), -2)
bb__      = fromPitch $ viaPitchL (6, (-1), -2)

cs___     = fromPitch $ viaPitchL (0, 1, -3)
ds___     = fromPitch $ viaPitchL (1, 1, -3)
es___     = fromPitch $ viaPitchL (2, 1, -3)
fs___     = fromPitch $ viaPitchL (3, 1, -3)
gs___     = fromPitch $ viaPitchL (4, 1, -3)
as___     = fromPitch $ viaPitchL (5, 1, -3)
bs___     = fromPitch $ viaPitchL (6, 1, -3)

c___      = fromPitch $ viaPitchL (0, 0, -3)
d___      = fromPitch $ viaPitchL (1, 0, -3)
e___      = fromPitch $ viaPitchL (2, 0, -3)
f___      = fromPitch $ viaPitchL (3, 0, -3)
g___      = fromPitch $ viaPitchL (4, 0, -3)
a___      = fromPitch $ viaPitchL (5, 0, -3)
b___      = fromPitch $ viaPitchL (6, 0, -3)

cb___     = fromPitch $ viaPitchL (0, (-1), -3)
db___     = fromPitch $ viaPitchL (1, (-1), -3)
eb___     = fromPitch $ viaPitchL (2, (-1), -3)
fb___     = fromPitch $ viaPitchL (3, (-1), -3)
gb___     = fromPitch $ viaPitchL (4, (-1), -3)
ab___     = fromPitch $ viaPitchL (5, (-1), -3)
bb___     = fromPitch $ viaPitchL (6, (-1), -3)

cs____    = fromPitch $ viaPitchL (0, 1, -4)
ds____    = fromPitch $ viaPitchL (1, 1, -4)
es____    = fromPitch $ viaPitchL (2, 1, -4)
fs____    = fromPitch $ viaPitchL (3, 1, -4)
gs____    = fromPitch $ viaPitchL (4, 1, -4)
as____    = fromPitch $ viaPitchL (5, 1, -4)
bs____    = fromPitch $ viaPitchL (6, 1, -4)

c____     = fromPitch $ viaPitchL (0, 0, -4)
d____     = fromPitch $ viaPitchL (1, 0, -4)
e____     = fromPitch $ viaPitchL (2, 0, -4)
f____     = fromPitch $ viaPitchL (3, 0, -4)
g____     = fromPitch $ viaPitchL (4, 0, -4)
a____     = fromPitch $ viaPitchL (5, 0, -4)
b____     = fromPitch $ viaPitchL (6, 0, -4)

cb____    = fromPitch $ viaPitchL (0, (-1), -4)
db____    = fromPitch $ viaPitchL (1, (-1), -4)
eb____    = fromPitch $ viaPitchL (2, (-1), -4)
fb____    = fromPitch $ viaPitchL (3, (-1), -4)
gb____    = fromPitch $ viaPitchL (4, (-1), -4)
ab____    = fromPitch $ viaPitchL (5, (-1), -4)
bb____    = fromPitch $ viaPitchL (6, (-1), -4)
