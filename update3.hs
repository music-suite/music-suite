
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    UndecidableInstances,
    GeneralizedNewtypeDeriving #-}

-- Copied from https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan
-- Simplified to capture a single type
import Control.Applicative
import Data.Semigroup
import Data.Foldable
import Data.Traversable

class HasPitch s where
  type Pitch             (s :: *) :: *
  getPitch :: (a ~ Pitch s) => s -> a


-- > set l (view l whole) whole == whole
-- > view l (set l part whole) == part
-- > set l part2 (set l part1) whole = set l part2 whole

-- SetPitch (Pitch s) s

class (HasPitch s, s ~ SetPitch (Pitch s) s) => UpdatePitch (b :: *) (s :: *) where
  type SetPitch (b :: *) (s :: *) :: *
  setPitch :: (b ~ Pitch t, t ~ SetPitch b s) => b -> s -> t

-- TODO always require this constraint, or make it a class for better type sigs
type UpdatePitch' s t a b = (UpdatePitch (Pitch t) s, SetPitch (Pitch t) s ~ t, a ~ Pitch s, b ~ Pitch t)
type UpdatePitch'' s a = UpdatePitch' s s a a

mapPitch :: (UpdatePitch' s t a b) => (a -> b) -> s -> t
mapPitch f x = setPitch p x where p = f (getPitch x)

incPitch :: (UpdatePitch'' s a, Enum a) => s -> s
incPitch = mapPitch succ


data PitchT f a = PitchT f a
    deriving (Show, Functor, Foldable, Traversable)

instance (Semigroup p, Monoid p) => Applicative (PitchT p) where
    pure = PitchT mempty
    PitchT pf vf <*> PitchT px vx = PitchT (pf <> px) (vf $ vx)

instance HasPitch (PitchT f a) where
    type Pitch      (PitchT f a) = f
    getPitch        (PitchT f a) = f

instance UpdatePitch g (PitchT f a)  where
    type SetPitch g (PitchT f a) = PitchT g a 
    setPitch      g (PitchT f a) = PitchT g a

instance HasPitch a => HasPitch [a] where
    type Pitch [a] = Pitch a
    getPitch [x] = getPitch x

-- Undecidable
instance (UpdatePitch b a) => UpdatePitch b [a] where
  type SetPitch b [a] = [SetPitch b a]
  setPitch b = fmap (setPitch b)

instance HasPitch a => HasPitch (c,a) where
    type Pitch (c,a) = Pitch a
    getPitch (c,a) = getPitch a
-- 
-- Undecidable ??
instance (UpdatePitch b a) => UpdatePitch b (c,a) where
  type SetPitch b (c,a) = (c,SetPitch b a)
  setPitch b = fmap (setPitch b)





(x,int2float) = (PitchT 3 (True, 0), fromIntegral)
int2float :: Int -> Float
x :: PitchT Int (Bool, Int)
y :: PitchT Float (Int, Bool)
y = fmap swap $ mapPitch (int2float) x
swap (x,y) = (y,x)
