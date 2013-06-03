
module Music.Lilypond.Pitch -- (
-- ) 
where


newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octaves) }
    deriving (Eq, Ord, Show)

data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum)

-- | For double flat -2, flat -1, natural 0, sharp 1 and double sharp 2.
type Accidental = Int 

-- | Number of octaves raised (positive) or flattened (negative).
type Octaves    = Int 

-- TODO use invervals Numeric.Inteval ?
type Interval = (Pitch, Pitch)

-- withPitch f = Pitch . f . getPitch
-- 
-- sharpen       = withPitch $ \(p,a,o) -> (p,a+1,o)
-- flatten       = withPitch $ \(p,a,o) -> (p,a-1,o)
-- shiftOctave x = withPitch $ \(p,a,o) -> (p,a,o+x)
-- 
-- pitches p o = (flatten x, x, sharpen x) where x = Pitch (p, 0, o)
-- --  Define all common pitches for convenience
-- 
-- cb_, c_, cs_ :: Pitch
-- db_, d_, ds_ :: Pitch
-- eb_, e_, es_ :: Pitch
-- fb_, f_, fs_ :: Pitch
-- gb_, g_, gs_ :: Pitch
-- ab_, a_, as_ :: Pitch
-- bb_, b_, bs_ :: Pitch
-- 
-- cb, c, cs :: Pitch
-- db, d, ds :: Pitch
-- eb, e, es :: Pitch
-- fb, f, fs :: Pitch
-- gb, g, gs :: Pitch
-- ab, a, as :: Pitch
-- bb, b, bs :: Pitch
-- 
-- cb', c', cs' :: Pitch
-- db', d', ds' :: Pitch
-- eb', e', es' :: Pitch
-- fb', f', fs' :: Pitch
-- gb', g', gs' :: Pitch
-- ab', a', as' :: Pitch
-- bb', b', bs' :: Pitch
-- 
-- (cb_, c_, cs_) = pitches C (-1)
-- (db_, d_, ds_) = pitches C (-1)
-- (eb_, e_, es_) = pitches C (-1)
-- (fb_, f_, fs_) = pitches C (-1)
-- (gb_, g_, gs_) = pitches C (-1)
-- (ab_, a_, as_) = pitches C (-1)
-- (bb_, b_, bs_) = pitches C (-1)
-- 
-- (cb, c, cs) = pitches C 0
-- (db, d, ds) = pitches C 0
-- (eb, e, es) = pitches C 0
-- (fb, f, fs) = pitches C 0
-- (gb, g, gs) = pitches C 0
-- (ab, a, as) = pitches C 0
-- (bb, b, bs) = pitches C 0
-- 
-- (cb', c', cs') = pitches C 1
-- (db', d', ds') = pitches C 1
-- (eb', e', es') = pitches C 1
-- (fb', f', fs') = pitches C 1
-- (gb', g', gs') = pitches C 1
-- (ab', a', as') = pitches C 1
-- (bb', b', bs') = pitches C 1
--                                
