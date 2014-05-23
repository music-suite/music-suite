
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides colored note heads.
--
-------------------------------------------------------------------------------------

module Music.Score.Color (
        -- ** HasColor class
        HasColor(..),
        -- * Manipulating color

        -- * Representation
        ColorT(..),
  ) where

import           Control.Applicative
import           Control.Lens                  hiding (above, below, transform)
import           Data.AffineSpace
import           Data.Colour
import qualified Data.Colour.Names             as C
import           Data.Foldable
import           Data.Semigroup
import           Data.Typeable

import           Music.Score.Part
import           Music.Time
import           Music.Time.Internal.Transform

import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Phrases
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo

deriving instance IsPitch a => IsPitch (ColorT a)
deriving instance IsDynamics a => IsDynamics (ColorT a)
deriving instance Transformable a => Transformable (ColorT a)
deriving instance Reversible a => Reversible (ColorT a)
deriving instance Alterable a => Alterable (ColorT a)
deriving instance Augmentable a => Augmentable (ColorT a)
-- TODO use Data.Color, not string
class HasColor a where
    setColor :: Colour Double -> a -> a

newtype ColorT a = ColorT { getColorT :: (Option (Last (Colour Double)), a) }
    deriving (Eq, {-Ord,-} Show, Functor, Foldable, {-Typeable, -}Applicative, Monad)
instance HasColor a => HasColor (b, a) where
    setColor       s                                 = fmap (setColor s)
instance HasColor a => HasColor [a] where
    setColor       s                                 = fmap (setColor s)
instance HasColor a => HasColor (Score a) where
    setColor       s                                 = fmap (setColor s)
instance HasColor a => HasColor (PartT n a) where
    setColor       s                                 = fmap (setColor s)
instance HasColor a => HasColor (TieT a) where
    setColor       s                                 = fmap (setColor s)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (ColorT a) where
  type Unwrapped (ColorT a) = (Option (Last (Colour Double)), a)
  _Wrapped' = iso getColorT ColorT
instance Rewrapped (ColorT a) (ColorT b)

instance HasColor (ColorT a) where
  setColor s (ColorT (t,x)) = ColorT (t <> wrap s,x)
    where
      wrap = Option . Just . Last

instance Semigroup a => Semigroup (ColorT a) where
    (<>) = liftA2 (<>)
instance Tiable a => Tiable (ColorT a) where
    toTied (ColorT (n,a))                         = (ColorT (n,b), ColorT (n,c)) where (b,c) = toTied a

