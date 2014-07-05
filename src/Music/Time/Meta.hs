
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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
-- Provides a way to annotate data-types with transformable meta-information.
--
-- Because meta-data is 'Transformable', it often makes sense to use 'Reactive' or 'Voice' wrappers
-- to represent time-varying information such as time signatures, global volume, arranger of a particular
-- section etc.
--
-- Inspired by Diagrams and Clojure.
--
-------------------------------------------------------------------------------------

module Music.Time.Meta (
        -- * Attributes
        IsAttribute,
        Attribute,
        
        -- ** Creating attributes
        wrapAttr,
        wrapTAttr,
        unwrapAttr,
        -- unwrapTAttr,

        -- * Meta-data
        Meta,

        -- ** Creating meta-data
        toMeta,
        fromMeta,

        -- ** The HasMeta class
        HasMeta(..),
        applyMeta,
        setMeta,
        setMetaAttr,
        setMetaTAttr,
        
        WithMeta,
        annotated,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens              hiding (transform)
import           Control.Monad.Plus
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String
-- import           Data.Traversable          (Traversable)
-- import qualified Data.Traversable          as T
import           Data.Typeable
import           Data.Void
import           Data.Functor.Couple

import           Music.Time.Internal.Util
import           Music.Time.Reverse
import           Music.Time.Split
import           Music.Time.Transform

type IsAttribute a = (Typeable a, Monoid a, Semigroup a)
type IsTAttribute a = (Transformable a, IsAttribute a)

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
  Attribute  :: IsAttribute a => a -> Attribute
  TAttribute :: IsTAttribute a  => a -> Attribute

wrapAttr :: IsAttribute a => a -> Attribute
wrapAttr = Attribute

wrapTAttr :: (Transformable a, IsAttribute a) => a -> Attribute
wrapTAttr = TAttribute

unwrapAttr :: IsAttribute a => Attribute -> Maybe a
unwrapAttr (Attribute a)  = cast a
unwrapAttr (TAttribute a) = cast a

-- Only needed if you want to transform and put back the meta
-- Maybe not event then?
-- unwrapTAttr :: (Transformable a, IsAttribute a) => Attribute -> Maybe a
-- unwrapTAttr (TAttribute a) = cast a

instance Semigroup Attribute where
  (Attribute a1) <> a2 = case unwrapAttr a2 of
    -- Nothing  -> a2
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> Attribute (a1 <> a2')
  (TAttribute a1) <> a2 = case unwrapAttr a2 of
    -- Nothing  -> a2
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> TAttribute (a1 <> a2')

instance Transformable Attribute where
  transform _ (Attribute a) = Attribute a
  transform s (TAttribute a) = TAttribute (transform s a)

instance Splittable Attribute where
  split _ x = (x,x)

instance Reversible Attribute where
  rev = id

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

fromMeta :: forall a b . ({-HasPart' a, -}IsAttribute b) => Maybe a -> Meta -> Maybe b
fromMeta partId (Meta s) = (unwrapAttr =<<) $ Map.lookup key s
-- Note: unwrapAttr should never fail
  where
    key = ty ++ pt
    pt = ""
    -- pt = show $ fmap getPart partId
    ty = show . typeOf $ (undefined :: b)

to_nonT_Meta :: forall b . ({-HasPart' a, -}IsAttribute b) => b -> Meta
to_nonT_Meta a = Meta $ Map.singleton key $ wrapAttr a
  where
    key = ty ++ pt
    pt = ""
    ty = show $ typeOf (undefined :: b)


-- | Type class for things which have meta-information.
class HasMeta a where
  -- | Apply meta-information by combining it (on the left) with the
  --   existing meta-information.
  meta :: Lens' a Meta

instance Show Meta where
  show _ = "{ meta }"

instance HasMeta Meta where
  meta = ($)

instance HasMeta b => HasMeta (b, a) where
  meta = _1 . meta

deriving instance HasMeta b => HasMeta (Twain b a)

-- TODO call withMeta a la Clojure?
setMeta :: HasMeta a => Meta -> a -> a
setMeta m = set meta m

applyMeta :: HasMeta a => Meta -> a -> a
applyMeta m = over meta (<> m)

setMetaAttr :: (IsAttribute b, HasMeta a) => b -> a -> a
setMetaAttr a = applyMeta (to_nonT_Meta a)

setMetaTAttr :: (IsAttribute b, Transformable b, HasMeta a) => b -> a -> a
setMetaTAttr a = applyMeta (toMeta Nothing a)




-- TODO Better name
-- TODO deriviations (esp of Eq, Ord, Num)
-- Meta should *not* affect Eq/Ord (as in Clojure)
newtype WithMeta a = WithMeta { getWithMeta :: Twain Meta a }
  deriving (
    Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad,
    Semigroup, Monoid, Num, Fractional, Floating, Enum, Bounded,
    Integral, Real, RealFrac,
    HasMeta, Eq, Ord
    )

annotated :: Iso' a (WithMeta a)
annotated = iso toWithMeta fromWithMeta
  where
    toWithMeta :: a -> WithMeta a
    toWithMeta = WithMeta . pure

    fromWithMeta :: WithMeta a -> a
    fromWithMeta = extract . getWithMeta


