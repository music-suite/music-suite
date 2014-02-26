
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


-- class HasGetPitch s where
--   getPitch :: (a ~ Pitch s) => s -> a
-- 
-- class (SetPitch (Pitch t) s ~ t) => HasSetPitch (s :: *) (t :: *) where
--   type SetPitch (b :: *) (s :: *) :: *
--   setPitch :: Pitch t -> s -> t
--   mapPitch :: (Pitch s -> Pitch t) -> s -> t
--   setPitch x = mapPitch (const x)
--   default mapPitch :: HasGetPitch s => (Pitch s -> Pitch t) -> s -> t
--   mapPitch f x = setPitch p x where p = f (getPitch x)
--   
-- type HasPitch s t = (HasGetPitch s, HasSetPitch s t)
-- 
-- type HasPitch' a = HasPitch a a
-- 
-- pitch' :: HasPitch' a => Lens' a (Pitch a)
-- pitch' = pitch
-- 
-- pitch :: HasPitch a b => Lens a b (Pitch a) (Pitch b)
-- pitch = lens getPitch (flip setPitch)
-- 
-- 
data PitchT f a = PitchT f a
    deriving (Show, Functor, Foldable, Traversable)

type instance Pitch (PitchT f a) = f
type instance SetPitch g (PitchT f a) = PitchT g a
instance HasPitch a b => HasPitch (PitchT a c) (PitchT b c) where
    pitch = lens (\(PitchT f a) -> f) (\(PitchT f a) g -> PitchT g a)
instance HasPitches a b => HasPitches (PitchT a c) (PitchT b c) where
    pitches = lens (\(PitchT f a) -> f) (\(PitchT f a) g -> PitchT g a)

-- 
-- instance (Semigroup p, Monoid p) => Applicative (PitchT p) where
--     pure = PitchT mempty
--     PitchT pf vf <*> PitchT px vx = PitchT (pf <> px) (vf $ vx)
-- 
-- type instance Pitch      (PitchT f a) = f
-- instance HasGetPitch (PitchT f a) where
--     getPitch        (PitchT f a) = f
-- 
-- instance HasSetPitch (PitchT f a) (PitchT g a)  where
--     type SetPitch g (PitchT f a) = PitchT g a 
--     setPitch      g (PitchT f a) = PitchT g a
-- 
-- -- Undecidable        
-- type instance Pitch [a] = Pitch a
-- instance HasSetPitch a b => HasSetPitch [a] [b] where
--     type SetPitch b [a] = [SetPitch b a]
--     mapPitch f = fmap (mapPitch f)
-- 
-- type instance Pitch (c,a) = Pitch a
-- instance HasGetPitch a => HasGetPitch (c,a) where
--     getPitch (c,a) = getPitch a
-- 
-- -- Undecidable ??
-- instance HasSetPitch a b => HasSetPitch (c,a) (c,b) where
--     type SetPitch b (c,a) = (c,SetPitch b a)
--     mapPitch f = fmap (mapPitch f)
-- 
-- 
-- 
-- -- instance HasGetPitch M.Pitch where
-- --     type Pitch M.Pitch = M.Pitch
-- --     getPitch = id
-- -- instance HasSetPitch M.Pitch a where
-- --     type SetPitch a M.Pitch = a
-- --     setPitch = const
-- 
-- type instance Pitch Int = Int
-- instance HasGetPitch Int where
--     getPitch = id
-- instance (a ~ Pitch a) => HasSetPitch Int a where
--     type SetPitch a Int = a
--     mapPitch = id
-- 
-- type instance Pitch Bool = Bool
-- instance HasGetPitch Bool where
--     getPitch = id
-- instance (a ~ Pitch a) => HasSetPitch Bool a where
--     type SetPitch a Bool = a
--     mapPitch = id
-- 
-- type instance Pitch M.Pitch = M.Pitch
-- instance HasGetPitch M.Pitch where
--     getPitch = id
-- instance (a ~ Pitch a) => HasSetPitch M.Pitch a where
--     type SetPitch a M.Pitch = a
--     mapPitch = id
-- 
-- 

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
