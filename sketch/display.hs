
{-# LANGUAGE FlexibleInstances #-}

import Music.Prelude
import qualified Data.Interval as I
import qualified TypeUnary.Nat

class Display a where
  display :: a -> IO ()
  display = open . displayS
  
  displayS :: a -> Score StandardNote
  displayS = error "Not implemnented: displayS"


instance Display Pitch where
  displayS = displayS . (pure :: a -> Score a)
instance Display Interval where
  displayS = (<> c) . fmap (fromPitch'.pure) . (pure :: a -> Score a) . (c .+^)  
instance Display [Pitch] where
  displayS = fmap (fromPitch'.pure) . compress 4 . scat . map (pure :: a -> Score a)  
instance Display (Ambitus Pitch) where
  displayS = displayS . ambitusShowPitches
instance TypeUnary.Nat.IsNat a => Display (Equal a) where
  displayS = displayS . equalToPitch
instance TypeUnary.Nat.IsNat a => Display [Equal a] where
  displayS = displayS . fmap equalToPitch

instance Display (Score Pitch) where
  displayS = fmap (fromPitch'.pure)
  -- TOOD hide dynamics etc
instance Display Dynamics where
  displayS x = level x c
instance Display Part where
  displayS x = set parts' x c
instance Display Instrument where
  displayS = displayS . tutti
instance Display String where
  displayS x = text x c
  

-- TODO move

-- | Pitches to show when drawing an interval.
-- Always return a list of 1 or 2 elements.
ambitusShowPitches :: Num a => Ambitus a -> [a]
ambitusShowPitches x = case (I.lowerBound x, I.upperBound x) of
  (I.Finite x, I.Finite y) -> [x,y]
  (_, I.Finite y) -> [y]
  (I.Finite x, _) -> [x]

ambitus :: Iso' (Pitch, Pitch) (Ambitus Pitch)
ambitus = iso f g
  where
    f (x,y) = I.interval (I.Finite x, True) (I.Finite y, True)
    g = undefined

-- TODO move
instance TypeUnary.Nat.IsNat a => Enum (Equal a) where
  toEnum = toEqual
  fromEnum = fromEqual
instance TypeUnary.Nat.IsNat a => Real (Equal a) where
  toRational = toRational . fromEqual
instance TypeUnary.Nat.IsNat a => Integral (Equal a) where
  toInteger = toInteger . fromEqual
  quotRem x y = (fromIntegral q, fromIntegral r)
    where (q, r) = quotRem (fromIntegral x) (fromIntegral y)

  
-- TODO reexport IsNat
equalToPitch :: TypeUnary.Nat.IsNat a => Equal a -> Pitch
equalToPitch = equal12ToPitch . toEqual12

toEqual12 :: TypeUnary.Nat.IsNat a => Equal a -> Equal12
toEqual12 = cast

equal12ToPitch :: Equal12 -> Pitch
equal12ToPitch = (c .+^) . spell modally . (fromIntegral :: Equal12 -> Semitones)

