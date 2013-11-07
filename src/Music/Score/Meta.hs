
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds, 
    GADTs, 
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
-- 
--
-------------------------------------------------------------------------------------


module Music.Score.Meta (
        IsAttribute,
        Attribute,
        wrapAttr,
        unwrapAttr,
        Meta,
        runMeta,
        HasMeta(..),

        TimeSignature,
        KeySignature,
        Tempo,
        Clef(..),
        setClef,
        setClefDuring,

        setTimeSignature,
        setTimeSignatureDuring,

        setKeySignature,
        setKeySignatureDuring,

        setTempo,
        setTempoDuring,
  ) where

import Data.Void
import Data.Maybe
import Data.Semigroup
import Control.Arrow
import Control.Monad.Plus       

import Data.Typeable
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


type IsAttribute a = (Typeable a, Semigroup a)

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
  (Attribute a1) <> a2 =
    case unwrapAttr a2 of
      Nothing  -> a2
      Just a2' -> Attribute (a1 <> a2')

instance Delayable Attribute where
  delay _ (Attribute  a) = Attribute a
instance Stretchable Attribute where
  stretch _ (Attribute  a) = Attribute a


-- TODO is Transformable right w.r.t. join?
newtype Meta = Meta (Map String (Reactive Attribute))
    deriving (Delayable, Stretchable)

inMeta :: (Map String (Reactive Attribute) -> Map String (Reactive Attribute)) -> Meta -> Meta
inMeta f (Meta s) = Meta (f s)

addM :: forall a b . (IsAttribute a, Monoid a, HasMeta b) => Note a -> b -> b
addM x = applyMeta $ addMeta $ noteToReact x

addMeta :: forall a . IsAttribute a => Reactive a -> Meta
addMeta a = Meta $ Map.singleton ty $ fmap wrapAttr a
    where
        ty = show $ typeOf (undefined :: a)

runMeta :: forall a . (Monoid a, IsAttribute a) => Meta -> Reactive a
runMeta = fromMaybe mempty . runMeta'

runMeta' :: forall a . IsAttribute a => Meta -> Maybe (Reactive a) 
runMeta' (Meta s) = fmap (fmap (fromMaybe (error "runMeta'") . unwrapAttr)) $ Map.lookup ty s
-- Note: unwrapAttr should never fail
    where
        ty = show . typeOf $ (undefined :: a)

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



data Clef = GClef | CClef | FClef
    deriving (Eq, Ord, Show, Typeable)

setClef :: (HasMeta a, HasOnset a, HasOffset a) => Clef -> a -> a
setClef c x = setClefDuring (onset x <-> offset x) c x

setClefDuring :: HasMeta a => Span -> Clef -> a -> a
setClefDuring s c = addM (s =: (Option $ Just $ Last c))


newtype TimeSignature = TimeSignature ([Integer], Integer)
    deriving (Eq, Ord, Show, Typeable)

setTimeSignature :: (HasMeta a, HasOnset a, HasOffset a) => TimeSignature -> a -> a
setTimeSignature c x = setTimeSignatureDuring (onset x <-> offset x) c x

setTimeSignatureDuring :: HasMeta a => Span -> TimeSignature -> a -> a
setTimeSignatureDuring s c = addM (s =: (Option $ Just $ Last c))

newtype KeySignature = KeySignature (Integer, Bool)
    deriving (Eq, Ord, Show, Typeable)

setKeySignature :: (HasMeta a, HasOnset a, HasOffset a) => KeySignature -> a -> a
setKeySignature c x = setKeySignatureDuring (onset x <-> offset x) c x

setKeySignatureDuring :: HasMeta a => Span -> KeySignature -> a -> a
setKeySignatureDuring s c = addM (s =: (Option $ Just $ Last c))

newtype Tempo = Tempo Duration
    deriving (Eq, Ord, Show, Typeable)

setTempo :: (HasMeta a, HasOnset a, HasOffset a) => Tempo -> a -> a
setTempo c x = setTempoDuring (onset x <-> offset x) c x

setTempoDuring :: HasMeta a => Span -> Tempo -> a -> a
setTempoDuring s c = addM (s =: (Option $ Just $ Last c))

