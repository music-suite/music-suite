
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
-- Provides meta-information.
--
-- Each score supports an unlimited number of 'Reactive' meta-values.
--
-- This is more or less based on Diagrams styles, which is in turn based
-- on XMonad.
--
-------------------------------------------------------------------------------------

module Music.Time.Meta (
        -- * Attributes
        IsAttribute,
        Attribute,
        wrapAttr,
        unwrapAttr,
        wrapTAttr,
        unwrapTAttr,

        -- * Meta-values
        Meta,
        toMeta,
        fromMeta,
        HasMeta(..),
        applyMeta,
  ) where

-- import           Control.Applicative
-- import           Control.Arrow
import           Control.Lens              hiding (transform)
import           Control.Monad.Plus
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String
-- import           Data.Traversable          (Traversable)
-- import qualified Data.Traversable          as T
import           Data.Typeable
import           Data.Void

-- import           Music.Score.Part
import           Music.Score.Util
import           Music.Time.Reverse
import           Music.Time.Split
import           Music.Time.Transform

type IsAttribute a = (Typeable a, Monoid' a)

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
  Attribute  :: IsAttribute a => a -> Attribute
  TAttribute :: (Transformable a, IsAttribute a) => a -> Attribute

wrapAttr :: IsAttribute a => a -> Attribute
wrapAttr = Attribute

unwrapAttr :: IsAttribute a => Attribute -> Maybe a
unwrapAttr (Attribute a) = cast a

wrapTAttr :: (Transformable a, IsAttribute a) => a -> Attribute
wrapTAttr = TAttribute

unwrapTAttr :: (Transformable a, IsAttribute a) => Attribute -> Maybe a
unwrapTAttr (TAttribute a) = cast a

instance Semigroup Attribute where
  (Attribute a1) <> a2 = case unwrapAttr a2 of
    -- Nothing  -> a2
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> Attribute (a1 <> a2')
  (TAttribute a1) <> a2 = case unwrapTAttr a2 of
    -- Nothing  -> a2
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> TAttribute (a1 <> a2')

instance Transformable Attribute where
  transform _ (Attribute a) = Attribute a
  transform s (TAttribute a) = TAttribute (transform s a)

instance Splittable Attribute where
  split = error "Not implemented (Splittable Attribute)"

instance Reversible Attribute where
  rev = error "Not implemented (Reversible Attribute)"

-- Meta is Transformable because the contents of the map is transformable
newtype Meta = Meta (Map String Attribute)
  deriving (Transformable, Reversible, Splittable)

instance Semigroup Meta where
  Meta s1 <> Meta s2 = Meta $ Map.unionWith (<>) s1 s2

-- | The empty meta contains no attributes; composition of metas is
--   a union of attributes; if the two metas have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Monoid Meta where
  mempty = Meta Map.empty
  mappend = (<>)

instance HasMeta Meta where
  meta = ($)

--
-- TODO
-- Temporarily disabling part specific meta-events
-- The API still works, but all parts are merged together
--

toMeta :: forall a b . ({-HasPart' a, -}IsAttribute b, Transformable b) => Maybe a -> b -> Meta
toMeta partId a = Meta $ Map.singleton key $ wrapTAttr a
  where
    key = ty ++ pt
    pt = ""
    -- pt = show $ fmap getPart partId
     ty = show $ typeOf (undefined :: b)

fromMeta :: forall a b . ({-HasPart' a, -}IsAttribute b, Transformable b) => Maybe a -> Meta -> Maybe b
fromMeta partId (Meta s) = (unwrapTAttr =<<) $ Map.lookup key s
-- Note: unwrapAttr should never fail
  where
    key = ty ++ pt
    pt = ""
    -- pt = show $ fmap getPart partId
    ty = show . typeOf $ (undefined :: b)


-- | Type class for things which have meta-information.
class HasMeta a where
  -- | Apply meta-information by combining it (on the left) with the
  --   existing meta-information.
  meta :: Lens' a Meta

applyMeta :: HasMeta a => Meta -> a -> a
applyMeta m = (meta <>~ m)

