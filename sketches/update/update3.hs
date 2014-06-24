
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
    GeneralizedNewtypeDeriving #-}

module Update3 where
    
-- Copied from https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan
-- Simplified to capture a single type
import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Data.AffineSpace -- tests
import qualified Music.Pitch as M

-- Constraint versions of the lens laws
-- type GetPut s     = (s ~ SetPitch (Pitch s) s)
-- type PutGet s a   = (a ~ Pitch (SetPitch a s))
-- type PutPut s a b = (b ~ SetPitch b (SetPitch a s))

class HasPitch s where
  type Pitch             (s :: *) :: *
  getPitch :: (a ~ Pitch s) => s -> a

class (HasPitch s, s ~ SetPitch (Pitch s) s) => UpdatePitch (b :: *) (s :: *) where
  type SetPitch (b :: *) (s :: *) :: *
  setPitch :: (b ~ Pitch t, t ~ SetPitch b s) => b -> s -> t

type HasPitch2 s t = (UpdatePitch (Pitch t) s, SetPitch (Pitch t) s ~ t)
type HasPitch' s = HasPitch2 s s


mapPitch :: (HasPitch2 s t) => (Pitch s -> Pitch t) -> s -> t
mapPitch f x = setPitch p x where p = f (getPitch x)

mapPitch' :: (HasPitch' s) => (Pitch s -> Pitch s) -> s -> s
mapPitch' = mapPitch



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
    -- TODO crashes when updating longer lists etc

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



instance HasPitch M.Pitch where
    type Pitch M.Pitch = M.Pitch
    getPitch = id
instance UpdatePitch M.Pitch M.Pitch where
    type SetPitch M.Pitch M.Pitch = M.Pitch
    setPitch = const



type Interval a = Diff (Pitch a)

up :: (UpdatePitch (Pitch t) t, AffineSpace (Pitch t)) => Interval t -> t -> t
up x = mapPitch (.+^ x)


(x,int2float) = (PitchT 3 (True, 0), fromIntegral)
int2float :: Int -> Float
x :: PitchT Int (Bool, Int)
y :: PitchT Float (Int, Bool)
y = fmap swap $ mapPitch (int2float) x
swap (x,y) = (y,x)
