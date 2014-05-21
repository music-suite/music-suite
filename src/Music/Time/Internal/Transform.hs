
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Music.Time.Internal.Transform (

      -- * Music.Time.Transform
      -- * The Transformable class
      Transformable(..),
      itransform,
      transformed,

      -- ** Apply under a transformation
      whilst,
      whilstM,
      whilstL,
      whilstLT,
      whilstLD,
      whilstStretch,
      whilstDelay,
      spanned,
      onS,
      conjugateS,

      -- ** Specific transformations
      delay,
      undelay,
      stretch,
      compress,
      -- *** Applied transformations
      delaying,
      undelaying,
      stretching,
      compressing,
      -- *** Utility
      delayTime,
  ) where

import           Music.Time.Types

import           Data.Ratio

import           Control.Applicative
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Semigroup
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.VectorSpace       hiding (Sum (..))

-- |
-- Class of values that can be transformed (i.e. scaled and moved) in time.
--
-- Law
--
-- @
-- transform mempty = id
-- transform (s \<> t) = transform s . transform t
-- transform (s \<> negateV t) = id
-- @
--
-- Law
--
-- @
-- onset (delay n a)       = n ^+. onset a
-- offset (delay n a)      = n ^+. offset a
-- duration (stretch n a)  = n * duration a
-- duration (compress n a) = duration a / n
-- @
--
-- @
-- delay n b ! t    = b ! (t .-^ n)
-- undelay n b ! t  = b ! (t .+^ n)
-- @
--
-- Lemma
--
-- @
-- duration a = duration (delay n a)
-- @
--
class Transformable a where
  transform :: Span -> a -> a

instance Transformable () where
  transform _ = id

instance Transformable Bool where
  transform _ = id

instance Transformable Ordering where
  transform _ = id

instance Transformable Char where
  transform _ = id

instance Transformable Int where
  transform _ = id

instance Transformable Integer where
  transform _ = id

instance Transformable a => Transformable (Ratio a) where
  transform _ = id

instance Transformable Float where
  transform _ = id

instance Transformable Double where
  transform _ = id

instance Transformable Duration where
  (view delta -> (_, d1)) `transform` d2 = d1 * d2

instance Transformable Time where
  (view delta -> (t1, d1)) `transform` t2 = t1 ^+^ d1 *^ t2

instance Transformable Span where
  transform = (<>)

instance Transformable a => Transformable (Option a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Last a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Sum a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Product a) where
  transform s = fmap (transform s)

-- | 
-- Apply the inverse of the given transformation.
--
-- @
-- 'itransform' s = 'transform' ('negateV' s)
-- @
--
itransform :: Transformable a => Span -> a -> a
itransform s = transform (negateV s)

-- | 
-- View the given value in the context of the given transformation.
--
-- @
-- 'over' ('transformed' s) = (``whilst`` s)
-- @
--
transformed :: Transformable a => Span -> Iso' a a
transformed s = iso (transform s) (itransform s)

--
-- TODO
--
-- Should really transform the /second/ element, but this is incompatible with Note/SCcore
--
-- 1) Change this to transform both components
--    Then Note could be defined as   type Note a = (Span, TransfInv a)
--
-- 2) Redefine note as                type Note a = (a, Span)
--
instance Transformable a => Transformable (b, a) where
  transform t (s,a) = (s, transform t a)

-- |
-- Lists transform by transforming each element.
--
instance Transformable a => Transformable [a] where
  transform t = map (transform t)

instance Transformable a => Transformable (Seq a) where
  transform t = fmap (transform t)

instance (Ord k, Transformable a) => Transformable (Map k a) where
  transform t = Map.map (transform t)

-- |
-- Functions transform by conjugation, i.e. we reverse-transform the argument
-- and transform the result.
--
instance (Transformable a, Transformable b) => Transformable (a -> b) where
  transform t = (`whilst` negateV t)

-- |
-- A transformation that moves a value forward in time.
--
delaying :: Duration -> Span
delaying x = (0 .+^ x) >-> 1

-- |
-- A transformation that stretches (augments) a value by the given factor.
--
stretching :: Duration -> Span
stretching x = 0 >-> x

-- |
-- A transformation that moves a value backward in time.
--
undelaying :: Duration -> Span
undelaying x = delaying (negate x)

-- |
-- A transformation that compresses (diminishes) a value by the given factor.
--
compressing :: Duration -> Span
compressing x = stretching (recip x)

-- |
-- Moves a value forward in time.
--
delay :: Transformable a => Duration -> a -> a
delay = transform . delaying

-- |
-- Moves a value backward in time. Equnitvalent to @'stretch' . 'negate'@.
--
undelay :: Transformable a => Duration -> a -> a
undelay = transform . undelaying

-- |
-- Stretches (augments) a value by the given factor.
--
stretch :: Transformable a => Duration -> a -> a
stretch = transform . stretching

-- |
-- Compresses (diminishes) a score. Equnitvalent to @'stretch' . 'recip'@.
--
compress :: Transformable a => Duration -> a -> a
compress = transform . compressing


-- |
-- Delay relative to 'origin'.
--
-- Provided for situations when we really want to use 'startAt', but the
-- type does not have an instance for 'HasPosition' and we can assume that
-- the value is starting at time zero.
--
delayTime :: Transformable a => Time -> a -> a
delayTime t = delay (t .-. 0)



--
-- $musicTimeSpanConstruct
--
-- - To convert a span to a pair, use @s^.'delta'@.
-- - To construct a span from a pair, use @(t, d)^.'from' 'delta'@.
--

--
-- $musicTimeSpanLaws
--
-- > forall s . id `whilst` s = id
-- > forall s . return `whilstM` s = return
-- > forall s . extract `whilstW` s = extract


-- We really must flip all these functions. To do:
--
--    1) Come up with some other name for the infix version
--    2) Acknowledge that this is a valid Lens (when flipped)
--
-- Perhaps we should call the inline version `whilst`, as in @f `whilst` delaying 2@?


-- |
-- Apply a function under transformation.
--
-- Designed to be used infix, as in
--
-- @
-- 'stretch' 2 ``whilst`` 'delaying' 2
-- @
--
whilst :: (Transformable a, Transformable b) => (a -> b) -> Span -> a -> b
f `whilst` t = transform (negateV t) . f . transform t

-- |
-- Apply a morphism under transformation (monadic version).
--

whilstM :: (Functor f, Transformable a, Transformable b) => (a -> f b) -> Span -> a -> f b
f `whilstM` t = fmap (transform (negateV t)) . f . transform t

{-
-- |
-- Apply a morphism under transformation (co-monadic version).
--
whilstW :: (Functor f, Transformable a, Transformable b) => (f a -> b) -> Span -> f a -> b
f `whilstW` t = transform (negateV t) . f . fmap (transform t)
-}
whilstL :: (Functor f, Transformable a, Transformable b)
  => LensLike f s t a b
  -> LensLike f (Span,s) (Span,t) a b
whilstL  l f (s,a) = (s,) <$> (l $ f `whilstM` s) a

whilstLT :: (Functor f, Transformable a, Transformable b)
  => LensLike f s t a b
  -> LensLike f (Time,s) (Time,t) a b
whilstLT l f (t,a) = (t,) <$> (l $ f `whilstM` (t >-> 1)) a

whilstLD :: (Functor f, Transformable a, Transformable b)
  => LensLike f s t a b
  -> LensLike f (Duration,s) (Duration,t) a b
whilstLD l f (d,a) = (d,) <$> (l $ f `whilstM` (0 >-> d)) a


-- |
-- Apply a function under transformation.
--
whilstDelay :: (Transformable a, Transformable b) => (a -> b) -> Time -> a -> b
whilstDelay     = flip (flip whilst . delaying . (.-. 0))

-- |
-- Apply a function under transformation.
--
whilstStretch :: (Transformable a, Transformable b) => (a -> b) -> Duration -> a -> b
whilstStretch = flip (flip whilst . stretching)

conjugateS :: Span -> Span -> Span
conjugateS t1 t2  = negateV t1 <> t2 <> t1


-- |
-- Transforms a lens of to a 'Transformable' type to act inside a transformation.
--
spanned :: (Transformable a, Transformable b) => Span -> Lens a b a b
spanned s = flip whilstM (negateV s)

-- |
-- Transforms a lens of to a 'Transformable' type to act inside a transformation.
--
-- Designed to be used infix, as in
--
-- @
-- l `on` (2 \<-> 3)
-- @
--
onS :: (Transformable a, Functor f) => LensLike' f a b -> Span -> LensLike' f a b
f `onS` s = spanned s . f
-- TODO name

-- TODO move!
deriving instance Functor Sum
deriving instance Functor Product

