
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
    GeneralizedNewtypeDeriving #-}

module Update5 where
    
-- Copied from https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan
-- Simplified to capture a single type
import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Data.AffineSpace -- tests
import Data.AffineSpace.Relative -- tests
import qualified Music.Pitch as M

class HasPitch s where
  type Pitch             (s :: *) :: *
  _pitch :: (a ~ Pitch s) => s -> a

class (HasPitch s, SetPitch (Pitch t) s ~ t) => HasPitch2 (s :: *) (t :: *) where
  type SetPitch (b :: *) (s :: *) :: *

  _setPitch :: Pitch t -> s -> t
  _setPitch x = _mapPitch (const x)
  
  _mapPitch :: (Pitch s -> Pitch t) -> s -> t
  _mapPitch f x = _setPitch p x where p = f (_pitch x)

type HasPitch' a = HasPitch2 a a
  
pitch :: HasPitch2 s t => Lens s t (Pitch s) (Pitch t)
pitch = lens _pitch (flip _setPitch)

pitch' :: HasPitch2 s s => Lens' s (Pitch s)
pitch' = pitch

_setPitch' :: HasPitch2 s s => Pitch s -> s -> s
_setPitch' = _setPitch

_mapPitch' :: HasPitch2 s s => (Pitch s -> Pitch s) -> s -> s
_mapPitch' = _mapPitch

data PitchT f a = PitchT f a
    deriving (Show, Functor, Foldable, Traversable)

instance (Semigroup p, Monoid p) => Applicative (PitchT p) where
    pure = PitchT mempty
    PitchT pf vf <*> PitchT px vx = PitchT (pf <> px) (vf $ vx)

instance HasPitch (PitchT f a) where
    type Pitch      (PitchT f a) = f
    _pitch        (PitchT f a) = f

instance HasPitch2 (PitchT f a) (PitchT g a)  where
    type SetPitch g (PitchT f a) = PitchT g a 
    _setPitch      g (PitchT f a) = PitchT g a

-- instance HasPitch a => HasPitch [a] where
--     type Pitch [a] = Pitch a
--     _pitch [x] = _pitch x
--     -- TODO crashes when updating longer lists etc
-- 
-- -- Undecidable
-- instance (HasPitch2 a b) => HasPitch2 [a] [b] where
--   type SetPitch b [a] = [SetPitch b a]
--   _setPitch b = fmap (_setPitch b)      

instance HasPitch a => HasPitch (c,a) where
    type Pitch (c,a) = Pitch a
    _pitch (c,a) = _pitch a

-- Undecidable ??
instance (HasPitch2 a b) => HasPitch2 (c,a) (c,b) where
  type SetPitch b (c,a) = (c,SetPitch b a)
  _setPitch b = fmap (_setPitch b)



-- instance HasPitch M.Pitch where
--     type Pitch M.Pitch = M.Pitch
--     _pitch = id
-- instance HasPitch2 M.Pitch a where
--     type SetPitch a M.Pitch = a
--     _setPitch = const

instance HasPitch Int where
    type Pitch Int = Int
    _pitch = id
instance (a ~ Pitch a) => HasPitch2 Int a where
    type SetPitch a Int = a
    _setPitch = const

instance HasPitch Bool where
    type Pitch Bool = Bool
    _pitch = id
instance (a ~ Pitch a) => HasPitch2 Bool a where
    type SetPitch a Bool = a
    _setPitch = const

instance HasPitch M.Pitch where
    type Pitch M.Pitch = M.Pitch
    _pitch = id
instance (a ~ Pitch a) => HasPitch2 M.Pitch a where
    type SetPitch a M.Pitch = a
    _setPitch = const



type Interval a = Diff (Pitch a)


x :: PitchT Int (Bool, Int)
x = PitchT 3 (True, 0)

y :: PitchT Float (Bool, Int)
y = x & pitch %~ fromIntegral

z :: PitchT Bool (Bool, Int)
z = x & pitch .~ True

up :: (HasPitch' a, AffineSpace (Pitch a)) => Interval a -> a -> a
up a = pitch %~ (.+^ a)

down :: (HasPitch' a, AffineSpace (Pitch a)) => Interval a -> a -> a
down a = pitch %~ (.-^ a)

-- interval :: (HasPitch' a) => a -> a -> Interval a
-- interval x y = x^.pitch `distance` y^.pitch

invert :: (HasPitch' a, AffineSpace (Pitch a)) => Pitch a -> a -> a
invert p = pitch %~ (reflectAround p)


--                         
