
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Music.Pitch.Relative.Number where


-- |
-- The number portion of an interval (i.e. second, third, etc).
--
-- Note that the interval number is always one step larger than number of steps spanned by
-- the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
-- over addition:
--
-- > number (a + b) = number a + number b - 1
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
                   