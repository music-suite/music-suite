{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
module Music.Time.Internal.Transform
  ( module Music.Time.Types,

    -- * The Transformable class
    Transformable (..),
    transformed,

    -- * Apply under a transformation
    whilst,
    whilstL,
    whilstLT,
    whilstLD,
    whilstDelay,
    whilstStretch,
    onSpan,

    -- * Specific transformations

    -- ** Transformations
    delaying,
    undelaying,
    stretching,
    compressing,
    conjugateS,

    -- ** Transforming values
    delay,
    undelay,
    stretch,
    compress,
  )
where

import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Data.AffineSpace
import Data.Functor.Couple
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import Data.Semigroup
import Data.Semigroup.Instances ()
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VectorSpace hiding (Sum (..))
import Music.Time.Types

-- |
-- Class of values that can be transformed (i.e. scaled and moved) in time.
--
-- ==== Laws
--
-- [/identity/]
--
--    @'transform' mempty = id@
--
-- [/compatibility/]
--
--    @'transform' (s <> t) = 'transform' s . 'transform' t@
--
-- In other words, 'transform' must be a left-action of 'Span' on @a@.
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
  (view onsetAndDuration -> (_, d1)) `transform` d2 = d1 * d2

instance Transformable Time where
  (view onsetAndDuration -> (t1, d1)) `transform` t2 = t1 ^+^ d1 *^ t2

instance Transformable Span where
  transform = (<>)

instance Transformable a => Transformable (Maybe a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Last a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Sum a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Product a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (b, a) where
  transform t = fmap (transform t)

deriving instance (Monoid b, Transformable a) => Transformable (Couple b a)

instance Transformable a => Transformable [a] where
  transform t = fmap (transform t)

instance Transformable a => Transformable (Seq a) where
  transform t = fmap (transform t)

instance (Ord a, Transformable a) => Transformable (Set a) where
  transform t = Set.map (transform t)

instance (Ord k, Transformable a) => Transformable (Map k a) where
  transform t = Map.map (transform t)

-- Functions transform by conjugation, i.e. we reverse-transform the argument
-- and transform the result.
--
instance (Transformable a, Transformable b) => Transformable (a -> b) where
  transform t = (`whilst` negateV t)
    where
      f `whilst` t = over (transformed t) f

-- |
-- View the given value in the context of the given transformation.
transformed :: (Transformable a, Transformable b) => Span -> Iso a b a b
transformed s = iso (transform s) (transform $ negateV s)

-- |
-- Apply a function under transformation.
--
-- >>> stretch 2 `whilst` delaying 2 $ (1 <-> 2)
-- 4 <-> 6
whilst :: (Transformable a, Transformable b) => (a -> b) -> Span -> a -> b
f `whilst` t = over (transformed t) f

-- f `whilst` t = transform (negateV t) . f . transform t

delayed :: (Transformable a, Transformable b) => Time -> Iso a b a b
delayed = transformed . delayingTime

stretched :: (Transformable a, Transformable b) => Duration -> Iso a b a b
stretched = transformed . stretching

-- |
-- A transformation that moves a value forward in time.
delaying :: Duration -> Span
delaying x = (0 .+^ x) >-> 1

delayingTime :: Time -> Span
delayingTime x = x >-> 1

-- |
-- A transformation that stretches (augments) a value by the given factor.
stretching :: Duration -> Span
stretching x = 0 >-> x

-- |
-- A transformation that moves a value backward in time.
undelaying :: Duration -> Span
undelaying x = delaying (negate x)

-- |
-- A transformation that compresses (diminishes) a value by the given factor.
compressing :: Duration -> Span
compressing x = stretching (recip x)

-- |
-- Moves a value forward in time.
delay :: Transformable a => Duration -> a -> a
delay = transform . delaying

-- |
-- Moves a value backward in time. Equnitvalent to @'stretch' . 'negate'@.
undelay :: Transformable a => Duration -> a -> a
undelay = transform . undelaying

-- |
-- Stretches (augments) a value by the given factor.
stretch :: Transformable a => Duration -> a -> a
stretch = transform . stretching

-- |
-- Compresses (diminishes) a score. Equnitvalent to @'stretch' . 'recip'@.
compress :: Transformable a => Duration -> a -> a
compress = transform . compressing

--

-- $musicTimeSpanConstruct
--
-- - To convert a span to a pair, use @s^.'onsetAndDuration'@.
-- - To construct a span from a pair, use @(t, d)^.'from' 'onsetAndDuration'@.

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

{-

-- flip whilstM is a lens
flip whilstM :: (Functor f, Transformable a, Transformable b) => (a -> f b) -> Span -> a -> f b
s `flip whilstM` f = fmap (transform (negateV t)) . f . transform t

-- is this the same as transformed?


From lens:
  iso sa bt = dimap sa (fmap bt)
From profunctor:
  dimap ab cd bc = cd . bc . ab
  dimap ab cd    = \f -> cd . f . ab


flip whilstM = transformed
flip whilstM = \s -> iso (transform s) (itransform s)
flip whilstM = \s -> dimap (transform s) (fmap $ itransform s)
flip whilstM = \s f -> (fmap $ itransform s) . f . transform s
flip (\f t -> fmap (transform (negateV t)) . f . transform t) = \s f -> (fmap $ itransform s) . f . transform s

\t f -> fmap (transform (negateV t)) . f . transform t
=
\t f -> (fmap $ itransform t) . f . transform t

\t f -> fmap (itransform t) . f . transform t
=
\t f -> fmap (itransform t) . f . transform t




Something similar to whilstL* is being used in Note/Delayed/Stretched
Are they the same?

whilstL l f (s,a)  = (s,) <$> (l $ transformed s f) a
whilstL id f (s,a) = (s,) <$> (transformed s f) a
whilstL id         = \f (s,a) -> (s,) <$> (transformed s f) a

whilstL id
  :: (Transformable a, Transformable b, Functor f) =>
     (a -> f b) -> (Span, a) -> f (Span, b)

-}

-- type LensLike (f :: * -> *) s t a b = (a -> f b) -> s -> f t

-- :: Functor f => (x -> afb -> afb') -> (afb' -> s -> f t) -> afb -> (x, s) -> f (x, t)
-- TODO rename
dobar :: Functor f => (x -> LensLike f a' b' a b) -> LensLike f s t a' b' -> LensLike f (x, s) (x, t) a b
dobar q l = \f (s, a) -> (s,) <$> (l (q s f)) a

whilstL ::
  (Functor f, Transformable a, Transformable b) =>
  LensLike f s t a b ->
  LensLike f (Span, s) (Span, t) a b
-- whilstL l = whilstL2 . l
whilstL l = dobar transformed l

{-
If we could rewrite (whilstL l) as (whilstLXX . l)

-}

whilstLT ::
  (Functor f, Transformable a, Transformable b) =>
  LensLike f s t a b ->
  LensLike f (Time, s) (Time, t) a b
whilstLT = dobar delayed

whilstLD ::
  (Functor f, Transformable a, Transformable b) =>
  LensLike f s t a b ->
  LensLike f (Duration, s) (Duration, t) a b
whilstLD = dobar stretched

-- |
-- Apply a function under transformation.
whilstDelay :: (Transformable a, Transformable b) => (a -> b) -> Time -> a -> b
whilstDelay = flip (flip whilst . delaying . (.-. 0))

-- |
-- Apply a function under transformation.
whilstStretch :: (Transformable a, Transformable b) => (a -> b) -> Duration -> a -> b
whilstStretch = flip (flip whilst . stretching)

-- |
-- The conjugate of two spans.
conjugateS :: Span -> Span -> Span
conjugateS t1 t2 = negateV t1 <> t2 <> t1

-- |
-- Transforms a lens of to a 'Transformable' type to act inside a transformation.
--
-- Designed to be used infix, as in
--
-- @
-- l `onSpan` (2 \<-> 3)
-- @
onSpan ::
  (Transformable s, Transformable t, Functor f) =>
  LensLike f s t a b ->
  Span ->
  LensLike f s t a b
f `onSpan` s = transformed (negateV s) . f
-- TODO name

-- deriving instance Functor Sum
-- deriving instance Functor Product
