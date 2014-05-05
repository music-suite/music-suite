
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
-- Provides clefs.
--
-------------------------------------------------------------------------------------


module Music.Score.Clef (
        ClefT(..),
        HasClef(..),
  ) where

import           Control.Arrow
import           Control.Lens hiding (transform, parts)
import           Control.Monad.Plus
import           Data.Foldable           (Foldable)
import qualified Data.Foldable           as F
import qualified Data.List               as List
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.String
import           Data.Traversable        (Traversable)
import qualified Data.Traversable        as T
import           Data.Typeable
import           Data.Void

-- import           Music.Score.Combinators (mapFirst)
-- import           Music.Score.Meta
import           Music.Score.Meta.Clef
import           Music.Score.Ornaments   (HasText, text)
import           Music.Score.Part
import           Music.Score.Ties
import           Music.Score.Util
import           Music.Time


-- Put the given clef in front of the note
newtype ClefT a = ClefT { getClefT :: (Option (Last Clef), a) }
    deriving (Functor, Semigroup, Monoid)

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (ClefT a) where
  type Unwrapped (ClefT a) = (Option (Last Clef), a)
  _Wrapped' = iso getClefT ClefT
instance Rewrapped (ClefT a) (ClefT b)

instance Monad ClefT where
    return x = ClefT (mempty, x)
    (>>=) = error "No ClefT.(>>=)"

type instance Part (ClefT a) = Part a
type instance SetPart b (ClefT a) = ClefT (SetPart b a)

instance (HasParts a b) => HasParts (ClefT a) (ClefT b) where
  parts = _Wrapped . parts
instance (HasPart a b) => HasPart (ClefT a) (ClefT b) where
  part = _Wrapped . part


instance Transformable a => Transformable (ClefT a) where
    transform s = over (_Wrapped . _2) $ transform s
  
instance Tiable a => Tiable (ClefT a) where
    toTied (ClefT (clef,a)) = (ClefT (clef,b), ClefT (mempty,c)) where (b,c) = toTied a

class HasClef a where
    applyClef :: Clef -> a -> a

instance HasClef (ClefT a) where
    applyClef c (ClefT (_,a)) = ClefT (Option $ Just $ Last c,a)

instance HasClef a => HasClef (b,a) where
    applyClef c = fmap (applyClef c)

instance (HasPart' a, HasClef a) => HasClef (Score a) where
    applyClef c = id -- TODO
    -- applyClef c = mapFirst (applyClef c) id

