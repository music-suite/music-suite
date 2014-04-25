
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
import           Music.Score.Meta
import           Music.Score.Meta.Clef
import           Music.Score.Ornaments   (HasText, text)
import           Music.Score.Part
import           Music.Score.Ties
import           Music.Score.Util
import           Music.Time


-- Put the given clef in front of the note
newtype ClefT a = ClefT { getClefT :: (Option (Last Clef), a) }
    deriving (Functor, Semigroup, Monoid)

type instance Part (ClefT a) = Part a
-- instance HasPart a => HasPart (ClefT a) where
--     getPart (ClefT (_,a)) = getPart a
--     modifyPart f (ClefT (a,b)) = ClefT (a, modifyPart f b)

instance Monad ClefT where
    return x = ClefT (mempty, x)
    (>>=) = error "No ClefT.(>>=)"


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

