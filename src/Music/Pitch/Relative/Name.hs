 
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Music.Pitch.Relative.Name where

instance Show Name where
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show A = "a"
    show B = "b"

data Name = C | D | E | F | G | A | B
    deriving (Eq, Ord, Enum)

