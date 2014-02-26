
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    NoMonomorphismRestriction,
    UndecidableInstances,
    RankNTypes,
    DefaultSignatures,
    GeneralizedNewtypeDeriving #-}

module Update6 where
    
-- Like Update5, this design is based on the Overloaded record fields proposal,
-- but simplified to capture a single class for each type (we are only interested
-- in four anyway)
--
--  See <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan>
--

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Data.AffineSpace
import Data.AffineSpace.Point
import qualified Music.Pitch as M

type family Pitch             (s :: *) :: * -- Pitch s   = a
type family SetPitch (b :: *) (s :: *) :: * -- Pitch b s = t

-- class Has s t a b | 
--     s -> a, 
--     -- t -> b, 
--     s b -> t, 
--     -- t a -> s

-- type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

class (SetPitch (Pitch t) s ~ t) => HasPitch s t where
    pitch :: Lens s t (Pitch s) (Pitch t)

    pitch' :: (s ~ t) => Lens' s (Pitch s)
    pitch' = pitch

class (SetPitch (Pitch t) s ~ t) => HasPitches s t where
    pitches :: Traversal s t (Pitch s) (Pitch t)

    pitches' :: (s ~ t) => Traversal' s (Pitch s)
    pitches' = pitches

{-
    lens id (flip const)
        = (\sa sbt afb s -> sbt s <$> afb (sa s)) id (flip const)
        = (\afb s -> (flip const) s <$> afb (id s))
        = (\afb s -> (flip const) s <$> afb s)
        = (\afb s -> id <$> afb s)
        = (\afb s -> afb s)
        = ($)
-}

type instance Pitch Bool = Bool
type instance SetPitch a Bool = a
instance (a ~ Pitch a) => HasPitch Bool a where
    pitch = ($)
instance (a ~ Pitch a) => HasPitches Bool a where
    pitches = ($)
    
type instance Pitch Int = Int
type instance SetPitch a Int = a
instance (a ~ Pitch a) => HasPitch Int a where
    pitch = ($)
instance (a ~ Pitch a) => HasPitches Int a where
    pitches = ($)

type instance Pitch Float = Float
type instance SetPitch a Float = a
instance (a ~ Pitch a) => HasPitch Float a where
    pitch = ($)
instance (a ~ Pitch a) => HasPitches Float a where
    pitches = ($)

type instance Pitch (c,a) = Pitch a
type instance SetPitch b (c,a) = (c,SetPitch b a)
instance HasPitch a b => HasPitch (c, a) (c, b) where
    pitch = _2 . pitch
instance HasPitches a b => HasPitches (c, a) (c, b) where
    pitches = traverse . pitches

type instance Pitch [a] = Pitch a
type instance SetPitch b [a] = [SetPitch b a]
instance HasPitches a b => HasPitches [a] [b] where
    pitches = traverse . pitches


data PitchT f a = PitchT f a
    deriving (Show, Functor, Foldable, Traversable)

type instance Pitch (PitchT p a) = p
type instance SetPitch p' (PitchT p a) = PitchT p' a
instance HasPitch a b => HasPitch (PitchT a c) (PitchT b c) where
    pitch = lens (\(PitchT p a) -> p) (\(PitchT p a) p' -> PitchT p' a)
instance HasPitches a b => HasPitches (PitchT a c) (PitchT b c) where
    pitches = lens (\(PitchT p a) -> p) (\(PitchT p a) p' -> PitchT p' a)


type Interval a = Diff (Pitch a)


x :: PitchT Int (Bool, Int)
x = PitchT 3 (True, 0)

y :: PitchT Float (Bool, Int)
y = x & pitch %~ fromIntegral

z :: PitchT Bool (Bool, Int)
z = x & pitch .~ True

-- up :: (HasPitches a a, AffineSpace (Pitch a)) => Interval a -> a -> a
up a = pitches %~ (.+^ a)

-- down :: (HasPitches a a, AffineSpace (Pitch a)) => Interval a -> a -> a
down a = pitches %~ (.-^ a)


type HasInterval a = (Floating (Interval a), M.InnerSpace (Interval a), M.Scalar (Interval a) ~ (Interval a))

interval :: (HasPitch a a, AffineSpace (Pitch a), HasInterval a) => a -> a -> Interval a
interval x y = (x^.pitch) `distance` (y^.pitch)

invert :: (HasPitch a a, AffineSpace (Pitch a)) => Pitch a -> a -> a
invert p = pitch %~ reflectThrough p      


--                         
