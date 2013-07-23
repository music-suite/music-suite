
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Music.Pitch.Relative.Accidental where

class Alterable a where
    -- | Increase the given pitch by one.
    sharpen :: a -> a
    -- | Decrease the given pitch by one.
    flatten :: a -> a

newtype Accidental = Accidental { getAccidental :: Integer }
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

