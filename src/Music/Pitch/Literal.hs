
module Music.Pitch.Literal (

        PitchL(..),
        Pitched(..),
        
        cs' , ds' , es' , fs' , gs' , as' , bs' ,
        c'  , d'  , e'  , f'  , g'  , a'  , b'  ,
        cb' , db' , eb' , fb' , gb' , ab' , bb' ,

        cs  , ds  , es  , fs  , gs  , as  , bs  ,
        c   , d   , e   , f   , g   , a   , b   ,
        cb  , db  , eb  , fb  , gb  , ab  , bb  ,

        cs_ , ds_ , es_ , fs_ , gs_ , as_ , bs_ ,
        c_  , d_  , e_  , f_  , g_  , a_  , b_  ,
        cb_ , db_ , eb_ , fb_ , gb_ , ab_ , bb_

  ) where

newtype PitchL = PitchL { fromPitchL :: (Int, Maybe Double, Int) }

-- Like Num can be expressed using arabic numerals, instances
-- of Pitched can be expressed using Western pitch names (c, c sharp, c flat etc)    
class Pitched a where
    fromPitch :: PitchL -> a

instance Pitched PitchL where
    fromPitch = id


cs' , ds' , es' , fs' , gs' , as' , bs' ::  Pitched a => a
c'  , d'  , e'  , f'  , g'  , a'  , b'  ::  Pitched a => a
cb' , db' , eb' , fb' , gb' , ab' , bb' ::  Pitched a => a

cs  , ds  , es  , fs  , gs  , as  , bs  ::  Pitched a => a
c   , d   , e   , f   , g   , a   , b   ::  Pitched a => a
cb  , db  , eb  , fb  , gb  , ab  , bb  ::  Pitched a => a

cs_ , ds_ , es_ , fs_ , gs_ , as_ , bs_ ::  Pitched a => a
c_  , d_  , e_  , f_  , g_  , a_  , b_  ::  Pitched a => a
cb_ , db_ , eb_ , fb_ , gb_ , ab_ , bb_ ::  Pitched a => a


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

