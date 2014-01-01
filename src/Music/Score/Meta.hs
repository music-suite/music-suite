
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

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

module Music.Score.Meta (
        -- * Attributes
        IsAttribute,
        Attribute,
        wrapAttr,
        unwrapAttr,

        -- * Meta-values
        Meta,  
        -- addMeta,
        addMetaNote,
        addGlobalMetaNote,
        runMeta,
        HasMeta(..),
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Plus       
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Util
import Music.Pitch.Literal


type IsAttribute a = (Typeable a, Monoid' a)

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
    Attribute  :: IsAttribute a => a -> Attribute
    -- TAttribute  :: (Transformable a, IsAttribute a) => a -> Attribute

-- | Wrap up an attribute.
wrapAttr :: IsAttribute a => a -> Attribute
wrapAttr = Attribute

unwrapAttr :: IsAttribute a => Attribute -> Maybe a
unwrapAttr (Attribute a)  = cast a

instance Semigroup Attribute where
    (Attribute a1) <> a2 = case unwrapAttr a2 of
        Nothing  -> a2
        Just a2' -> Attribute (a1 <> a2')

instance Delayable Attribute where
    delay _ (Attribute  a) = Attribute a
instance Stretchable Attribute where
    stretch _ (Attribute  a) = Attribute a


-- TODO is Transformable right w.r.t. join?
newtype Meta = Meta (Map String (Reactive Attribute))
    deriving (Delayable, Stretchable)

-- instance HasPart Meta where

inMeta :: (Map String (Reactive Attribute) -> Map String (Reactive Attribute)) -> Meta -> Meta
inMeta f (Meta s) = Meta (f s)


addGlobalMetaNote :: forall a b . (IsAttribute a, HasMeta b) => Note a -> b -> b
addGlobalMetaNote x = applyMeta $ addMeta' (Nothing::Maybe Int) $ noteToReactive x

-- XXX
addMetaNote :: forall a b . (IsAttribute a, HasMeta b, HasPart' b) => Note a -> b -> b
addMetaNote x y = (applyMeta $ addMeta' (Just y) $ noteToReactive x) y

-- Switch at time t to the given value (switch is valid until the end of the music).
-- TODO might not work as we think
addMetaChange :: forall a b . (IsAttribute a, HasMeta b, HasPart' b) => Time -> a -> b -> b
addMetaChange t x y = (applyMeta $ addMeta' (Just y) $ switch t mempty (pure x)) y


runMeta :: forall a b . (HasPart' a, IsAttribute b) => Maybe a -> Meta -> Reactive b
runMeta part = fromMaybe mempty . runMeta' part

addMeta' :: forall a b . (HasPart' a, IsAttribute b) => Maybe a -> Reactive b -> Meta
addMeta' part a = Meta $ Map.singleton key $ fmap wrapAttr a
    where                   
        key = ty ++ pt
        pt = show $ fmap getPart part
        ty = show $ typeOf (undefined :: b)

-- runMeta' :: forall a . IsAttribute a => Meta -> Maybe (Reactive a) 
runMeta' :: forall a b . (HasPart' a, IsAttribute b) => Maybe a -> Meta -> Maybe (Reactive b) 
runMeta' part (Meta s) = fmap (fmap (fromMaybe (error "runMeta'") . unwrapAttr)) $ Map.lookup key s
-- Note: unwrapAttr should never fail
    where
        key = ty ++ pt
        pt = show $ fmap getPart part
        ty = show . typeOf $ (undefined :: b)

instance Semigroup Meta where
    Meta s1 <> Meta s2 = Meta $ Map.unionWith (<>) s1 s2

-- | The empty meta contains no attributes; composition of metas is
--   a union of attributes; if the two metas have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Monoid Meta where
    mempty = Meta Map.empty
    mappend = (<>)

-- | Type class for things which have meta-information.
class HasMeta a where
    -- | Apply meta-information by combining it (on the left) with the
    --   existing meta-information.
    applyMeta :: Meta -> a -> a

instance HasMeta Meta where
    applyMeta = mappend

instance (HasMeta a, HasMeta b) => HasMeta (a,b) where
    applyMeta s = applyMeta s *** applyMeta s

instance HasMeta a => HasMeta [a] where
    applyMeta = fmap . applyMeta

instance HasMeta b => HasMeta (a -> b) where
    applyMeta = fmap . applyMeta

instance HasMeta a => HasMeta (Map k a) where
    applyMeta = fmap . applyMeta

instance (HasMeta a, Ord a) => HasMeta (Set a) where
    applyMeta = Set.map . applyMeta










newtype RehearsalMark = RehearsalMark ()
    deriving (Typeable, Monoid, Semigroup)









