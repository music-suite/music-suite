
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving #-}

-- Copied from https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan
-- Simplified to capture a single type

type family Wrapped             (s :: *) :: *
type family SetWrapped (a :: *) (s :: *) :: *

class HasWrapped s where
  getWrapped :: (a ~ Wrapped s) => s -> a

class (HasWrapped s, s ~ SetWrapped (Wrapped s) s) => UpdateWrapped (b :: *) (s :: *) where
  setWrapped :: (b ~ Wrapped t, t ~ SetWrapped b s) => b -> s -> t



data Wrap a = Wrap { getWrap :: [a] }
type instance Wrapped   (Wrap a)         = [a]
type instance SetWrapped [b] (Wrap nope) = Wrap b

instance HasWrapped (Wrap a) where 
  getWrapped (Wrap x) = x

instance UpdateWrapped [b] (Wrap a)  where
  setWrapped x (Wrap _) = Wrap x

mapWrapped :: (a ~ Wrapped s, b ~ Wrapped t, UpdateWrapped b s, SetWrapped b s ~ t) => (a -> b) -> s -> t
mapWrapped f x = (flip setWrapped x . f . getWrapped) x