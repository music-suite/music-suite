
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

module Update5 where
    
{-
This design is based on the Overloaded record fields proposal,
but simplified to capture a single class for each type (we are only interested
in four anyway)

 See <https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan>

One thing I like about this definition is how the type and value-level functions
correspond directly. There are some problems:

    - The canonical names pitch and pitch' requre a full lens, while in reality
      some types provides Traversals (i.e. Setter and Fold but no Getter), while other
      provide full lenses (Getter and Setter but no Fold).

-}

import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Data.AffineSpace
import Data.AffineSpace.Point
-- import qualified Music.Pitch as M

type family Pitch (s :: *) :: *
class HasGetPitch s where
  getPitch :: (a ~ Pitch s) => s -> a

class (SetPitch (Pitch t) s ~ t) => HasSetPitch (s :: *) (t :: *) where
  type SetPitch (b :: *) (s :: *) :: *
  setPitch :: Pitch t -> s -> t
  mapPitch :: (Pitch s -> Pitch t) -> s -> t
  setPitch x = mapPitch (const x)
  default mapPitch :: HasGetPitch s => (Pitch s -> Pitch t) -> s -> t
  mapPitch f x = setPitch p x where p = f (getPitch x)
  
type HasPitch s t = (HasGetPitch s, HasSetPitch s t)

type HasPitch' a = HasPitch a a

pitch' :: HasPitch' a => Lens' a (Pitch a)
pitch' = pitch

pitch :: HasPitch a b => Lens a b (Pitch a) (Pitch b)
pitch = lens getPitch (flip setPitch)

-- setPitch' :: HasSetPitch s s => Pitch s -> s -> s
-- setPitch' = setPitch

-- mapPitch' :: HasSetPitch s s => (Pitch s -> Pitch s) -> s -> s
-- mapPitch' = mapPitch

data PitchT f a = PitchT f a
    deriving (Show, Functor, Foldable, Traversable)

instance (Semigroup p, Monoid p) => Applicative (PitchT p) where
    pure = PitchT mempty
    PitchT pf vf <*> PitchT px vx = PitchT (pf <> px) (vf $ vx)

type instance Pitch (PitchT f a) = f
instance HasGetPitch (PitchT f a) where
    getPitch        (PitchT f a) = f

instance HasSetPitch (PitchT f a) (PitchT g a)  where
    type SetPitch g (PitchT f a) = PitchT g a 
    setPitch      g (PitchT f a) = PitchT g a

type instance Pitch [a] = Pitch a
instance (HasSetPitch a b) => HasSetPitch [a] [b] where
    type SetPitch b [a] = [SetPitch b a]
    mapPitch f = fmap (mapPitch f)      

type instance Pitch (c,a) = Pitch a
instance HasGetPitch a => HasGetPitch (c,a) where
    getPitch (c,a) = getPitch a

-- Undecidable ??
instance (HasGetPitch a, HasSetPitch a b) => HasSetPitch (c,a) (c,b) where
    type SetPitch b (c,a) = (c,SetPitch b a)
    mapPitch f = fmap (mapPitch f)      



-- instance HasGetPitch M.Pitch where
--     type Pitch M.Pitch = M.Pitch
--     getPitch = id
-- instance HasSetPitch M.Pitch a where
--     type SetPitch a M.Pitch = a
--     setPitch = const

type instance Pitch Int = Int
instance HasGetPitch Int where
    getPitch = id
instance HasSetPitch Int a where
    type SetPitch a Int = a
    setPitch = const

type instance Pitch Bool = Bool
instance HasGetPitch Bool where
    getPitch = id
instance (a ~ Pitch a) => HasSetPitch Bool a where
    type SetPitch a Bool = a
    setPitch = const

-- type instance Pitch M.Pitch = M.Pitch
-- instance HasGetPitch M.Pitch where
--     getPitch = id
-- instance (a ~ Pitch a) => HasSetPitch M.Pitch a where
--     type SetPitch a M.Pitch = a
--     setPitch = const



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


-- type HasInterval a = (Floating (Interval a), M.InnerSpace (Interval a), M.Scalar (Interval a) ~ (Interval a))

-- interval :: (HasPitch' a, AffineSpace (Pitch a), HasInterval a) => a -> a -> Interval a
-- interval x y = (x^.pitch) `distance` (y^.pitch)
-- 
-- invert :: (HasPitch' a, AffineSpace (Pitch a)) => Pitch a -> a -> a
-- invert p = pitch %~ reflectThrough p


--                         
