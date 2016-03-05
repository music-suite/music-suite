
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (
  main,
  
  Reactive,
  initial,
  final,
  intermediate,
  discrete,
  updates,
  occs,
  atTime,
  -- splitReactive,
  switchR,
  trimR,
  continous,
  continousWith,
  sample,
) where

import Music.Time (Note)
import Music.Prelude.Standard hiding (
  F,
  Note,
  Reactive,
  Behavior,
  initial,
  final,
  intermediate,
  discrete,
  updates,
  occs,
  atTime,
  splitReactive,
  switchR,
  trimR,
  continous,
  continousWith,
  sample,

  )

main = putStrLn "Main"

-- TODO move and derive
instance Monoid a => Monoid (Delayed a)
  where
  mempty =  pure mempty
  mappend = liftA2 mappend



data ConstOp f a = K a | F (f a)
  deriving (Functor)

instance Applicative f => Applicative (ConstOp f) where
  pure        = K
  K f <*> K x = K $ f x
  F f <*> K x = F $ f <*> pure x
  K f <*> F x = F $ pure f <*> x
  F f <*> F x = F $ f <*> x
  
  -- f <*> k = undefined





data Reactive a = Reactive a (Delayed (Voice a)) a
  deriving (Functor)

instance Semigroup a => Semigroup (Reactive a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Reactive a) where
  mempty =  pure mempty
  mappend = liftA2 mappend

instance Applicative Reactive where
  pure  = pureR
  (<*>) = apR ($)

pureR :: a -> Reactive a
-- pureR x = Reactive x mempty x
pureR = flip fmap unitR . const

unitR :: Reactive ()
unitR = undefined

zipR :: Reactive a -> Reactive b -> Reactive (a,b)
zipR = undefined

apR :: (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
apR f = fmap (fmap (fmap (uncurry f))) zipR
-- apR f = (fmap (uncurry f) .) . zipR






initial :: Reactive a -> a
initial (Reactive a v b) = a

final :: Reactive a -> a
final (Reactive a v b) = b

intermediate :: Transformable a => Reactive a -> [Note a]
intermediate = undefined

updates :: Reactive a -> [(Time, a)]
updates = undefined

resets :: Reactive a -> [(a, Time)]
resets = undefined

occs :: Reactive a -> [Time]
occs = fmap fst . updates

atTime :: Reactive a -> Time -> a
atTime = undefined

switchR :: Time -> Reactive a -> Reactive a -> Reactive a
switchR = undefined
-- switchR t 
--   (Reactive b1 v1 e1) 
--   (Reactive b2 v2 e2) 
--   = Reactive b1 () e2

trimR :: Monoid a => Span -> Reactive a -> Reactive a
trimR (view range -> (t1, t2)) x = switchR t1 mempty (switchR t2 x mempty)



{-
data Behavior a = Behavior (Reactive (Either a (Segment a)))

discrete :: Reactive a -> Behavior a
discrete = Behavior . fmap Left

continous :: Reactive (Segment a) -> Behavior a
continous = Behavior . fmap Right

continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

sample :: [Time] -> Behavior a -> Reactive a
sample = undefined
-}

data Behavior a = Behavior (Reactive (ConstOp Segment a))

discrete :: Reactive a -> Behavior a
discrete = Behavior . fmap K

continous :: Reactive (Segment a) -> Behavior a
continous = Behavior . fmap F

continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

sample :: [Time] -> Behavior a -> Reactive a
sample = undefined



