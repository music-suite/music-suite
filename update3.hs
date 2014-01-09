
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    UndecidableInstances,
    GeneralizedNewtypeDeriving #-}

-- Copied from https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan
-- Simplified to capture a single type


class HasPitch s where
  type Pitch             (s :: *) :: *
  getPitch :: (a ~ Pitch s) => s -> a

class (HasPitch s, s ~ SetPitch (Pitch s) s) => UpdatePitch (b :: *) (s :: *) where
  type SetPitch (b :: *) (s :: *) :: *
  setPitch :: (b ~ Pitch t, t ~ SetPitch b s) => b -> s -> t

mapPitch :: (UpdatePitch b s, SetPitch b s ~ t, a ~ Pitch s, b ~ Pitch t) => (a -> b) -> s -> t
mapPitch f x = setPitch p x where p = f (getPitch x)




data Wrap a = Wrap { getWrap :: a }

instance HasPitch (Wrap a) where 
  type Pitch   (Wrap a) = a
  getPitch     (Wrap a) = a

instance UpdatePitch b (Wrap a)  where
  type SetPitch b (Wrap a) = Wrap b
  setPitch      b (Wrap a) = Wrap b
                                  


data PitchT f a = PitchT f a
    deriving (Show)

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





(xs,int2float) = undefined
int2float :: Int -> Float
xs :: Wrap Int
ys :: Wrap Float
ys = mapPitch (int2float) xs
