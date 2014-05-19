


{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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
-------------------------------------------------------------------------------------


module Music.Score.Color (
        -- ** HasColor class
        HasColor(..),
        -- * Manipulating color        

        -- * Representation        
        ColorT(..),
  ) where

import           Control.Applicative
import Control.Lens hiding (above, below, transform)
import           Data.AffineSpace
import           Data.VectorSpace        hiding (Sum)
import           Data.Foldable
import           Data.Semigroup
import           Data.Typeable

import Music.Time
import Music.Time.Internal.Transform
-- import           Music.Dynamics.Literal
-- import           Music.Pitch.Literal
-- import           Music.Score.Combinators
import           Music.Score.Part
-- import           Music.Score.Score

import Music.Score.Part
import Music.Score.Ornaments -- TODO
import Music.Score.Ties -- TODO
import           Music.Pitch.Literal
import           Music.Dynamics.Literal
import           Music.Score.Phrases

deriving instance IsPitch a => IsPitch (ColorT a)
deriving instance IsDynamics a => IsDynamics (ColorT a)
deriving instance Transformable a => Transformable (ColorT a)
deriving instance Reversible a => Reversible (ColorT a)
-- deriving instance Alterable a => Alterable (ColorT a)
-- deriving instance Augmentable a => Augmentable (ColorT a)
-- TODO use Data.Color, not string
class HasColor a where
    setColor :: String -> a -> a

newtype ColorT a = ColorT { getColorT :: ([String], a) }
    deriving (Eq, Show, Ord, Functor, Foldable, {-Typeable, -}Applicative, Monad)
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
  type Unwrapped (ColorT a) = ([String], a)
  _Wrapped' = iso getColorT ColorT
instance Rewrapped (ColorT a) (ColorT b)
instance HasColor (ColorT a) where
    setColor s (ColorT (t,x)) = ColorT ([s],x)
instance Semigroup a => Semigroup (ColorT a) where
    (<>) = liftA2 (<>)
instance Tiable a => Tiable (ColorT a) where
    toTied (ColorT (n,a))                         = (ColorT (n,b), ColorT (n,c)) where (b,c) = toTied a

