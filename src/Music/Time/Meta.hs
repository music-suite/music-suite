
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
-- Because meta-data is 'Transformable', it often makes sense to use 'Reactive' or 'Behavior'
-- wrappers to represent time-varying information.
--
-- See "Music.Score.Meta" for more specific applications.
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
        toTMeta,
        fromMeta,

        -- ** The HasMeta class
        HasMeta(..),
        setMeta,
        setMetaAttr,
        setMetaTAttr,
        applyMeta,
        
        AddMeta,
        annotated,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens              hiding (transform)
import           Control.Monad.Plus
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Functor.Adjunction  (unzipR)
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

toTMeta :: forall a. IsTAttribute a => a -> Meta
toTMeta a = Meta $ Map.singleton key $ wrapTAttr a
  where
    key = show $ typeOf (undefined :: a)

fromMeta :: forall a. IsAttribute a => Meta -> Maybe a
fromMeta (Meta s) = (unwrapAttr =<<) $ Map.lookup key s
-- Note: unwrapAttr should never fail
  where
    key = show . typeOf $ (undefined :: a)

-- Also works with transformabel attributes
toMeta :: forall a. IsAttribute a => a -> Meta
toMeta a = Meta $ Map.singleton key $ wrapAttr a
  where
    key = show $ typeOf (undefined :: a)


-- | Type class for things which have meta-data.
class HasMeta a where
  -- | Access the meta-data.
  meta :: Lens' a Meta

instance Show Meta where
  show _ = "{ meta }"

instance HasMeta Meta where
  meta = ($)

instance HasMeta a => HasMeta (Maybe a) where
  meta = lens viewM $ flip setM
    where
      viewM Nothing  = mempty
      viewM (Just x) = view meta x
      setM m = fmap (set meta m)

-- TODO arguably both of these are wrong
instance HasMeta b => HasMeta (b, a) where
  meta = _1 . meta

deriving instance HasMeta b => HasMeta (Twain b a)

-- TODO call withMeta a la Clojure?
setMeta :: HasMeta a => Meta -> a -> a
setMeta m = set meta m

-- | Apply meta-information by combining it (on the left) with the
--   existing meta-information.
applyMeta :: HasMeta a => Meta -> a -> a
applyMeta m = over meta (<> m)

setMetaAttr :: (IsAttribute b, HasMeta a) => b -> a -> a
setMetaAttr a = applyMeta (toMeta a)

setMetaTAttr :: (IsAttribute b, Transformable b, HasMeta a) => b -> a -> a
setMetaTAttr a = applyMeta (toTMeta a)




-- TODO Better name

-- |
-- Annotate an arbitrary type with meta-data, preserving instances of
-- all common type classes.
--
-- You can access the meta-data using 'meta', and the annotated value using 'annotated'.
--
newtype AddMeta a = AddMeta { getAddMeta :: Twain Meta a }
  deriving (
    Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad,
    Semigroup, Monoid, Num, Fractional, Floating, Enum, Bounded,
    Integral, Real, RealFrac,
    HasMeta, Eq, Ord
    )

-- instance FunctorWithIndex i AddMeta where
  -- imap f = over annotated $ imap f
-- 
-- instance FoldableWithIndex Span Score where
--   ifoldMap f (Score (m,x)) = ifoldMap f x
-- 
-- instance TraversableWithIndex Span Score where
--   itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x

instance Transformable a => Transformable (AddMeta a) where
  transform t = over meta (transform t) . over annotated (transform t)

instance Reversible a => Reversible (AddMeta a) where
  rev = over meta rev . over annotated rev

instance Splittable a => Splittable (AddMeta a) where
  split t = unzipR . fmap (split t)
  -- TODO need to split the meta too?

instance HasPosition a => HasPosition (AddMeta a) where
  _onset    = _onset . extract
  _offset   = _offset . extract
  _position = _position . extract

instance HasDuration a => HasDuration (AddMeta a) where
  _duration = _duration . extract


-- |
--
-- @
-- over annotated = fmap
-- @
-- 
-- TODO this is as unsafe as the unsafe... methods in Score etc
-- 
annotated :: Iso' a (AddMeta a)
annotated = iso toAddMeta fromAddMeta
  where
    toAddMeta :: a -> AddMeta a
    toAddMeta = AddMeta . pure

    fromAddMeta :: AddMeta a -> a
    fromAddMeta = extract . getAddMeta


