
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Delayed (
        -- * Placed type
        Placed,

        -- * Construction
        placed,
        placee,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens            hiding (Indexable, Level, above, below,
                                          index, inside, parts, reversed,
                                          transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Foldable           (Foldable)
import qualified Data.Foldable           as Foldable
import           Data.Functor.Adjunction (unzipR)
import           Data.Functor.Couple
import           Data.String
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- 'Placed' represents a value with an offset in time.
--
-- A placed value has a known 'position', but no 'duration'.
--
-- Placing a value inside 'Placed' does not make it invariant under 'stretch', as the
-- offset of a placed value may be stretched with respect to the origin. However, in
-- contrast to a note the /duration/ is not stretched.
--
newtype Placed a = Placed { getPlaced :: Time `Couple` a }
  deriving (
    Eq,
    Ord,
    Typeable,
    Foldable,
    Traversable,
    
    Functor,
    Applicative,
    Monad,
    Comonad    
    )

instance (Show a, Transformable a) => Show (Placed a) where
  show x = show (x^.from placed) ++ "^.placed"

instance Wrapped (Placed a) where
  type Unwrapped (Placed a) = (Time, a)
  _Wrapped' = iso (getCouple . getPlaced) (Placed . Couple)

instance Rewrapped (Placed a) (Placed b)

instance Transformable a => Transformable (Placed a) where
  transform t = 
    over (from placed . _1) (transform t) 
    . 
    over (from placed . _2) (stretch $ stretchComponent t)

instance IsString a => IsString (Placed a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Placed a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Placed a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Placed a) where
  fromDynamics = pure . fromDynamics

placed :: Iso (Time, a) (Time, b) (Placed a) (Placed b)
placed = _Unwrapped

placee :: (Transformable a, Transformable b, b ~ a) => Lens (Placed a) (Placed b) a b
-- placee = from placed `dependingOn` (transformed . delayingTime)
placee = lens runPlaced $ flip (mapPlaced . const)
  where
    runPlaced :: Transformable a => Placed a -> a
    runPlaced = uncurry (\t -> transform (t >-> 1)) . view _Wrapped

    mapPlaced :: (Transformable a, Transformable b) => (a -> b) -> Placed a -> Placed b
    mapPlaced f (Placed (Couple (t,x))) = Placed $ Couple (t, over (transformed (t >-> 1)) f x)



dependingOn :: Lens' s (x,a) -> (x -> Lens' a c) -> Lens' s c
dependingOn l f = lens getter setter
  where
    getter s = let
      (x,a) = view l s
      l2    = f x
      in view l2 a
    setter s b = let
      (x,_) = view l s
      l2    = f x
      in set (l._2.l2) b s

